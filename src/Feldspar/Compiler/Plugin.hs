{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

-- | Dynamically load a compiled Feldspar function as a Haskell function
module Feldspar.Compiler.Plugin
  ( loadFun
  , loadFunWith
  , loadFunOpts
  , loadFunOptsWith
  , loadFunWithConfig
  , defaultConfig
  , pack   -- from MultiStage
  , unpack -- from MultiStage
  )
  where

import GHCi.ObjLink (initObjLinker, loadObj, resolveObjs)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
import GHCi.ObjLink (ShouldRetainCAFs(..))
#endif
import GHC.Paths (ghc)
import System.Plugins.MultiStage
import Distribution.Verbosity (verbose)
import Distribution.Simple.Utils (defaultPackageDesc)
import Distribution.PackageDescription
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
#endif
import Distribution.PackageDescription.Configuration (flattenPackageDescription)

import Feldspar.Compiler.CallConv (rewriteType, buildCType, buildHaskellType)

import Data.Default -- FIXME: Not required any longer?
import Foreign.Ptr
import Foreign.Marshal (with)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString)

import Control.Exception (handle)
import Control.Monad (join, (>=>), when, unless)

import Language.Haskell.TH hiding (Type, Range)

import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.Info (os)
import System.IO.Error (IOError)
import System.IO.Unsafe (unsafePerformIO)

-- Feldspar specific
import Feldspar.Runtime
import Feldspar.Compiler (compile, defaultOptions)
import Feldspar.Compiler.Backend.C.Options (Options(..))
import Feldspar.Compiler.Backend.C.Library (encodeFunctionName)
import Feldspar.Compiler.Marshal ()

-- | Configurable configuration for the loader.
feldsparPluginConfigWith :: String -> Options -> Config
feldsparPluginConfigWith suff fopts =
    feldsparPluginConfig { builder = feldsparBuilder fopts
                         , suffix  = suff
                         }

-- | Default configuration for the loader
feldsparPluginConfig :: Config
feldsparPluginConfig =
    defaultConfig { builder      = feldsparBuilder defaultOptions
                  , worker       = feldsparWorker
                  , typeFromName = loadFunType >=> rewriteType
                  , mkHSig       = buildHaskellType
                  , mkCSig       = buildCType
                  }

-- | Compile and load a Feldspar function into the current GHC session.
--
-- > prog1 :: Data Index -> Vector1 Index
-- > prog1 c = indexed c (const c)
-- >
-- > $(loadFun 'prog1)
--
-- The call to @loadFun@ above will splice code into the current module
-- to compile, load and wrap a Feldspar function as a Haskell function:
--
-- > c_prog1 :: Index -> [Index]
--
loadFun :: [Name] -> Q [Dec]
loadFun = loadFunWithConfig feldsparPluginConfig

-- | @loadFun@ with a function suffix to avoid collisions and different
--  feldspar-compiler options.
loadFunWith :: String -> Options -> [Name] -> Q [Dec]
loadFunWith s o = loadFunWithConfig (feldsparPluginConfigWith s o)

-- | Call @loadFun@ with C compiler options
loadFunOpts :: [String] -> [Name] -> Q [Dec]
loadFunOpts o = loadFunWithConfig feldsparPluginConfig {opts = o}

-- | Call @loadFunWith@ with C compiler options
loadFunOptsWith :: String -> Options -> [String] -> [Name] -> Q [Dec]
loadFunOptsWith pref fopt o =
    loadFunWithConfig (feldsparPluginConfigWith pref fopt){opts = o}

feldsparWorker :: Name -> [Name] -> Q Body
feldsparWorker fun as = normalB
    [|with def $ \outPtr -> do
        join $(infixApp (apply ([|pure $(varE fun)|] : map toRef as)) [|(<*>)|] [|pure outPtr|])
        peek outPtr >>= from
    |]
  where
    toRef name = [| pack $(varE name) |]

    apply :: [ExpQ] -> ExpQ
    apply [] = error "apply []"
    apply [x] = x
    apply (x:y:zs) = apply (infixApp x [|(<*>)|] y : zs)

feldsparBuilder :: Options -> Config -> Name -> Q Body
feldsparBuilder fopts Config{..} fun = do
    let db    = getDB
        opts' = opts ++ map ("-I"++) db
    normalB [|unsafeLocalState $ do
                createDirectoryIfMissing True wdir
                $(varE 'compile) $(varE fun) basename base fopts
                compileAndLoad basename opts'
                lookupSymbol symbol
            |]
  where
    base     = nameBase fun ++ suffix
    basename = wdir ++ "/" ++ base
    symbol   = ldprefix ++ encodeFunctionName base
    ldprefix = case os of
                 "darwin" -> "_"
                 _        -> ""

getDB :: [String]
getDB = unsafePerformIO $ do
    dirs <- sequence [ sandbox, user, local ]
    putStrLn $ unwords $ "Using feldspar runtime in" : concat dirs
    return $ concat dirs
  where
    sandbox = handle (\(_ :: IOError) -> return []) $ do
      (c,d,_) <- readProcessWithExitCode "cabal" ["sandbox", "hc-pkg","field","feldspar-language","include-dirs"] ""
      case c of
        ExitSuccess -> return $ drop 1 $ words d
        _           -> return []
    user = handle (\(_ :: IOError) -> return []) $ do
      (c,d,_) <- readProcessWithExitCode "ghc-pkg" ["field","feldspar-language","include-dirs"] ""
      case c of
        ExitSuccess -> return $ drop 1 $ words d
        _           -> return []
    local   = do
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
      pd <- readGenericPackageDescription verbose =<< defaultPackageDesc verbose
#else
      pd <- readPackageDescription verbose =<< defaultPackageDesc verbose
#endif
      let f a = return $ includeDirs $ libBuildInfo a
      maybe (return []) f (maybeHasLibs $ flattenPackageDescription pd)
{-# NOINLINE getDB #-}

maybeHasLibs :: PackageDescription -> Maybe Library
maybeHasLibs p =
   library p >>= \lib -> if buildable (libBuildInfo lib)
                           then Just lib
                           else Nothing

compileAndLoad :: String -> [String] -> IO ()
compileAndLoad name opts = do
    let cname = name ++ ".c"
        oname = name ++ ".o"
    exists <- doesFileExist oname
    when exists $ removeFile oname
    compileC cname oname opts
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 802
    initObjLinker RetainCAFs
#else
    initObjLinker
#endif
    _ <- loadObj oname
    res <- resolveObjs
    unless res $ error $ "Symbols in " ++ oname ++ " could not be resolved"

compileC :: String -> String -> [String] -> IO ()
compileC srcfile objfile opts = do
    let args = [ "-optc -std=c99"
               , "-optc -Wall"
               , "-w"
               , "-c"
               ]
    (_,stdout,stderr) <- readProcessWithExitCode ghc (args ++ opts ++ ["-o",objfile,srcfile]) ""
    let output = stdout ++ stderr
    unless (null output) $ putStrLn output

lookupSymbol :: String -> IO (Ptr a)
lookupSymbol symbol = do
    when (0 /= feldspar_compiler_hook) $ error "lookupSymbol: Runtime library missing"
    mptr <- withCString symbol _lookupSymbol
    when (mptr == nullPtr) $ error $ "Symbol " ++ symbol ++ " not found"
    return mptr

foreign import ccall safe "lookupSymbol"
    _lookupSymbol :: CString -> IO (Ptr a)
