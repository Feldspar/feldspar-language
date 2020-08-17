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
  , compileC
  )
  where

import GHCi.ObjLink (ShouldRetainCAFs(..), initObjLinker, loadObj, resolveObjs)
import GHC.Paths (ghc)
import System.Plugins.MultiStage hiding (ref)

import Data.Default
import Foreign.Ptr
import Foreign.Marshal (with)
import Foreign.Marshal.Unsafe (unsafeLocalState)
import Foreign.Storable (Storable(..))
import Foreign.C.String (CString, withCString)

import Control.Monad (join, (>=>), when, unless)

import Language.Haskell.TH

import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>))
import System.Process (readProcessWithExitCode)
import System.Info (os)

-- Feldspar specific
import Feldspar.Core.Reify (Syntactic(..))
import Feldspar.Compiler (compile)
import Feldspar.Compiler.Marshal ()
import Feldspar.Compiler.Options (Options(..), Platform(..), defaultOptions,
                                  encodeFunctionName)
import Paths_feldspar_language (getLibDir)

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

-- | Normalize the type (expand type synonyms and type families)
rewriteType :: Type -> Q Type
rewriteType = applyTF ''Internal

haskellCC :: CallConv
haskellCC = CallConv { arg  = return
                     , res  = appT (conT ''IO) . return
                     }

feldsparCC :: CallConv
feldsparCC = CallConv { arg = ref . rep . return
                      , res = toIO . appT (conT ''Ptr) . rep . return
                      }
  where
    ref    = appT (conT ''Ref)
    rep    = appT (conT ''Rep)
    toIO t = appT (appT arrowT t) (appT (conT ''IO) (tupleT 0))

-- | Construct the corresponding Haskell type of a foreign Feldspar
-- function
--
-- > prog1 :: Data Index -> Vector1 Index
-- >
-- > sigD (mkName "h_prog1") $ loadFunType 'prog1 >>= rewriteType >>= buildHaskellType
--
-- becomes
--
-- > h_prog1 :: Index -> IO [Index]
--
buildHaskellType :: Type -> Q Type
buildHaskellType = buildType haskellCC

-- | Construct the corresponding C type of a compiled Feldspar function
--
-- > sigD (mkName "c_prog1_fun") $ loadFunType 'prog1 >>= rewriteType
--                                                    >>= buildCType
--
-- becomes
--
-- > c_prog1_fun :: Word32 -> Ptr (SA Word32) -> IO ()
--
buildCType :: Type -> Q Type
buildCType = buildType feldsparCC

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
    normalB [|unsafeLocalState $ do
                createDirectoryIfMissing True wdir
                $(varE 'compile) $(varE fun) base fopts{outFileName = basename}
                compileAndLoad fopts basename opts
                lookupSymbol symbol
            |]
  where
    base     = nameBase fun ++ suffix
    basename = wdir </> base
    symbol   = ldprefix ++ encodeFunctionName base
    ldprefix = case os of
                 "darwin" -> "_"
                 _        -> ""

compileAndLoad :: Options -> String -> [String] -> IO ()
compileAndLoad opts name args = do
    let cname = name <.> "c"
        oname = name <.> "o"
    compileC opts cname oname args
    initObjLinker RetainCAFs
    _ <- loadObj oname
    res <- resolveObjs
    unless res $ error $ "Symbols in " ++ oname ++ " could not be resolved"

-- | Compile a Feldspar C file
compileC :: Options -> String -> String -> [String] -> IO ()
compileC opts srcfile objfile args' = do
    incDir <- fmap (</> "include") getLibDir
    let args = [ "-w", "-c", "-o", objfile, srcfile, "-I" ++ incDir]
               ++ map ("-optc " ++) (compilerFlags $ platform opts)
               ++ args'
    (ex, stdout, stderr) <- readProcessWithExitCode ghc args ""
    case ex of
      ExitFailure{} -> error $ unlines [show ex, stdout, stderr]
      _ -> do let output = stdout ++ stderr
              unless (null output) $ putStrLn output

lookupSymbol :: String -> IO (Ptr a)
lookupSymbol symbol = do
    mptr <- withCString symbol _lookupSymbol
    when (mptr == nullPtr) $ error $ "Symbol " ++ symbol ++ " not found"
    return mptr

foreign import ccall safe "lookupSymbol"
    _lookupSymbol :: CString -> IO (Ptr a)
