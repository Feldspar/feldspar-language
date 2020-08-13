--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- FIXME: Move the Pretty instances to the right place.
{-# OPTIONS_GHC -Wno-orphans #-}

module Feldspar.Compiler
  ( frontend
  , translate
  , reifyFeld
  , renameExp
  , compile
  , compileUT
  , icompile
  , icompileWith
  , icompile'
  , icompileFile
  , compileFile
  , program
  , programOpts
  , programOptsArgs
  ) where

import Control.Monad (when, unless)
import Control.Monad.State (evalState)
import qualified Data.ByteString.Char8 as B
import Data.Char (toUpper)
import Data.Foldable (forM_)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.FilePath ((<.>), takeFileName)
import System.IO (BufferMode(..), IOMode(..), hClose, hPutStr,
                  hSetBuffering, openFile)

import Feldspar.Compiler.Backend.C.CodeGeneration
import qualified Feldspar.Compiler.Backend.C.MachineLowering as ML
import Feldspar.Compiler.Backend.C.Tic64x
import Feldspar.Compiler.Imperative.ExternalProgram (parseFile)
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Imperative.ArrayOps
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Options
import Feldspar.Core.AdjustBindings (adjustBindings)
import Feldspar.Core.Middleend.CreateTasks
import Feldspar.Core.Middleend.Expand (expand)
import Feldspar.Core.Middleend.FromTyped (toU)
import Feldspar.Core.Middleend.LetSinking
import Feldspar.Core.Middleend.OptimizeUntyped
import Feldspar.Core.Middleend.PassManager (Prog(..), addSkip, addWrAfter,
                                            addWrBefore, evalPasses, passC,
                                            passT, setStopAfter, setStopBefore)
import Feldspar.Core.Middleend.PushLets
import Feldspar.Core.Middleend.UniqueVars
import qualified Feldspar.Core.SizeProp as SP
import Feldspar.Core.Representation (AExpr)
import Feldspar.Core.Reify (ASTF, Syntactic(..), desugar, unASTF)
import Feldspar.Core.UntypedRepresentation (UntypedFeld, prettyExp, rename)
import Feldspar.Core.ValueInfo (PrettyInfo(..), ValueInfo)

-- The front-end driver.

instance PrettyInfo a => Pretty (UntypedFeld a) where
  pretty = prettyExp f
     where f t x = " | " ++ prettyInfo t x

-- | Front-end driver
frontend :: Options
         -> Either
              (ASTF a)
              (Either
                (AExpr a)
                (Either
                  (UntypedFeld ValueInfo)
                  (Either
                    Module
                    (Either (Module, Module) SplitModule))))
         -> ([String], Maybe SplitModule)
frontend opts = evalPasses 0
                   ( codegen opts
                   . pc BPAdapt    (either (Left . adaptTic64x opts) Right)
                   . pc BPRename   (either (Left . ML.rename opts) Right)
                   . pc BPArrayOps (either (Left . arrayOps opts) Right)
                   . pt BPFromCore (either (Left . fromCoreUT opts) id)
                   . pc FPCreateTasks      (either (Left . createTasks opts) Right)
                   . pc FPUnique           (either (Left . uniqueVars) Right)
                   . pc FPExpand           (either (Left . expand) Right)
                   . pc FPPushLets         (either (Left . pushLets) Right)
                   . pc FPOptimize         (either (Left . optimize) Right)
                   . pc FPSinkLets         (either (Left . sinkLets opts) Right)
                   . pc FPRename           (either (Left . renameExp) Right)
                   . pt FPUntype           (either (Left . toU) id)
                   . pc FPSizeProp         (either (Left . SP.sizeProp) Right)
                   . pc FPAdjustBind       (either (Left . adjustBindings) Right)
                   . pt FPUnASTF           (either (Left . unASTF) id)
                   )
  where pc :: Pretty a => Pass -> (a -> a) -> Prog a Int -> Prog a Int
        pc = passC (passCtrl opts)
        pt :: (Pretty a, Pretty b)
           => Pass -> (a -> b) -> Prog a Int -> Prog b Int
        pt = passT (passCtrl opts)

reifyFeld :: Syntactic a => a -> ASTF (Internal a)
reifyFeld = desugar

renameExp :: UntypedFeld a -> UntypedFeld a
renameExp e = evalState (rename e) 0

compile :: Syntactic t => t -> String -> Options -> IO ()
compile prg funName opts = writeFiles opts compRes
  where compRes = compileToCCore funName opts prg

compileUT :: UntypedFeld ValueInfo -> String -> Options -> IO ()
compileUT prg funName opts = writeFiles opts compRes
  where compRes = fromMaybe (error "compileUT: compilation failed")
                $ snd $ frontend opts' (Right . Right . Left $ prg)
        opts' = opts{functionName = funName}

writeFiles :: Options -> SplitModule -> IO ()
writeFiles opts prg
  | "c" == codeGenerator (platform opts) = do
    writeFile cfile $ unlines [ "#include \"" ++ takeFileName hfile ++ "\""
                              , "\n"
                              , sourceCode $ implementation prg
                              ]
    writeFile hfile $ withIncludeGuard $ sourceCode $ interface prg
  | otherwise = writeFile (outFileName opts) $ sourceCode $ implementation prg
  where
    hfile = outFileName opts <.> "h"
    cfile = outFileName opts <.> "c"

    withIncludeGuard code = unlines [ "#ifndef " ++ guardName
                                    , "#define " ++ guardName
                                    , ""
                                    , code
                                    , ""
                                    , "#endif // " ++ guardName
                                    ]

    guardName = map ((\c -> if c `elem` toBeChanged then '_' else c) . toUpper) hfile
      where
        toBeChanged = "./\\"

icompile :: Syntactic t => t -> IO ()
icompile = icompileWith defaultOptions

icompileWith :: Syntactic t => Options -> t -> IO ()
icompileWith opts = icompile' opts "test"

icompile' :: Syntactic t => Options -> String -> t -> IO ()
icompile' opts fName prg = do
    let res = compileToCCore fName opts prg
    when (printHeader opts) $ do
      putStrLn "=============== Header ================"
      putStrLn $ sourceCode $ interface res
      putStrLn "=============== Source ================"
    putStrLn $ sourceCode $ implementation res

-- | Compile a C file and print the result
icompileFile :: FilePath -> IO ()
icompileFile fileName
  = compileFileHelper defaultOptions fileName
      (\_ (SplitModule cprg _) -> putStrLn $ sourceCode cprg)

-- | Compile a C file and write the result to a file
compileFile :: FilePath -> Options -> IO ()
compileFile fileName opts = compileFileHelper opts fileName writeFiles

-- | Helper function containing IO for the compileFile family of functions
compileFileHelper :: Options -> FilePath
                  -> (Options -> SplitModule -> IO ()) -> IO ()
compileFileHelper opts fileName f = do
  let hfilename = fileName <.> "h"
      cfilename = fileName <.> "c"
  h <- B.readFile hfilename
  c <- B.readFile cfilename
  f opts $ compileFile' opts (hfilename, h) (cfilename, c)

-- | Pure function for parsing and compiling C source code
compileFile' :: Options -> (String, B.ByteString) -> (String, B.ByteString)
            -> SplitModule
compileFile' opts (hfilename, hfile) (cfilename, cfile) =
  case parseFile hfilename hfile [] of
    Nothing -> error $ "Could not parse " ++ hfilename
    Just hprg -> case parseFile cfilename cfile (entities hprg) of
                   Nothing -> error $ "Could not parse " ++ cfilename
                   Just cprg -> fromMaybe (error "Failed parsing C file")
                                $ snd $ frontend opts
                                   (Right . Right . Right . Left $ cprg)

program :: Syntactic a => a -> IO ()
program p = programOpts p defaultOptions

programOpts :: Syntactic a => a -> Options -> IO ()
programOpts p opts = do args <- getArgs
                        programOptsArgs p opts args

programOptsArgs :: Syntactic a => a -> Options -> [String] -> IO ()
programOptsArgs p = programComp (const (return p))

programComp :: Syntactic a => ([String] -> IO a) -> Options -> [String] -> IO ()
programComp pc opts args = do
  name' <- getProgName
  let (opts1, nonopts) = decodeOpts (optsFromName opts name') args
      header = "Usage: " ++ name' ++ " <option>...\n"
                         ++ "where <option> is one of"
  if printHelp opts1
    then putStr $ usageInfo header driverOpts ++ passInfo ++ targetInfo
    else do
      p <- pc nonopts
      let (strs,mProgs) = translate opts1 p
      unless (null strs) $ writeFileLB (passFileName opts1) (concat strs)
      forM_ mProgs (writeFiles opts1)

optsFromName :: Options -> String -> Options
optsFromName opts name' = opts{ passFileName = name' <.> "passes"
                              , outFileName = name'
                              , functionName = name'}

decodeOpts :: Options -> [String] -> (Options, [String])
decodeOpts optsIn argv
  | null errors = (foldl (\o f -> f o) optsIn actions, nonOptions)
  | otherwise = error $ unlines errors
   where (actions, nonOptions, errors) = getOpt Permute driverOpts argv

passInfo :: String
passInfo = "\nPASS is one of\n" ++
           unlines (map ((++) "  " . unwords) $ chunksOf 5 $ map show fps)
  where chunksOf _ [] = []
        chunksOf n xs = take n xs:chunksOf n (drop n xs)
        fps = [minBound .. maxBound :: Pass]

targetInfo :: String
targetInfo = "\nTARGET is one of " ++ names ++ "\n\n"
  where names = unwords (map platformName availablePlatforms)

driverOpts :: [OptDescr (Options -> Options)]
driverOpts =
  [ Option []  ["writeBefore"]
    (ReqArg (chooseEnd addWrBefore) "PASS")   "write IR before PASS"
  , Option []  ["writeAfter"]
    (ReqArg (chooseEnd addWrAfter) "PASS")    "write IR after PASS"
  , Option []  ["stopBefore"]
    (ReqArg (chooseEnd setStopBefore) "PASS") "stop processing before PASS"
  , Option []  ["stopAfter"]
    (ReqArg (chooseEnd setStopAfter) "PASS")  "stop processing after PASS"
  , Option []  ["skip"]
    (ReqArg (chooseEnd addSkip) "PASS")       "skip PASS"
  , Option "o" ["outFile"]
    (ReqArg (\arg opts -> opts{outFileName = arg}) "FILE") "set base name of out file"
  , Option "p" ["passFile"]
    (ReqArg (\arg opts -> opts{passFileName = arg}) "FILE") "set name of pass file"
  , Option []  ["funcName"]
    (ReqArg (\arg opts -> opts{functionName = arg}) "IDENTIFIER") "set name of generated function"
  , Option "t" ["target"]
    (ReqArg (flip setTarget) "TARGET") "set target"
  , Option "h" ["help"]
    (NoArg (\opts -> opts{printHelp = True})) "print a useage message"
  ]

setTarget :: Options -> String -> Options
setTarget opts str = opts{platform = platformFromName str}

chooseEnd :: (PassCtrl -> Pass -> PassCtrl) -> String -> Options -> Options
chooseEnd f str opts
  | [(p,_)] <- reads str = opts{passCtrl = f (passCtrl opts) p}
  | otherwise = error $ "Compiler.chooseEnd: unrecognized pass " ++ str

writeFileLB :: String -> String -> IO ()
writeFileLB  "-"  str = putStr str
writeFileLB name' str = do fh <- openFile name' WriteMode
                           hSetBuffering fh LineBuffering
                           hPutStr fh str
                           hClose fh

translate :: Syntactic a => Options -> a -> ([String], Maybe SplitModule)
translate opts p = frontend opts $ Left $ reifyFeld p

data SplitModule = SplitModule
    { implementation :: CompiledModule
    , interface :: CompiledModule
    }

data CompiledModule = CompiledModule {
    sourceCode      :: String,
    debugModule     :: Module
}

-- | Split a module into interface and implemenation.
splitModule :: Module -> (Module, Module)
splitModule m = (Module (hdr ++ createProcDecls (entities m)), Module body)
  where
    (hdr, body) = partition belongsToHeader (entities m)
    belongsToHeader :: Entity -> Bool
    belongsToHeader StructDef{}                     = True
    belongsToHeader Proc{..} | Nothing <- procBody  = True
    belongsToHeader _                               = False
    -- TODO These only belongs in the header iff the types are used in a
    -- function interface
    createProcDecls :: [Entity] -> [Entity]
    createProcDecls = concatMap defToDecl
    defToDecl :: Entity -> [Entity]
    defToDecl (Proc n False inp rtype _) = [Proc n False inp rtype Nothing]
    defToDecl _ = []

compileSplitModule :: Options -> (Module, Module) -> SplitModule
compileSplitModule opts (hmdl, cmdl)
  = SplitModule
    { interface = CompiledModule { sourceCode  = incls ++ hres
                                 , debugModule = hmdl
                                 }
    , implementation = CompiledModule { sourceCode  = cres
                                      , debugModule = cmdl
                                      }
    }
  where
    hres = compToCWithInfos opts hmdl
    cres = compToCWithInfos opts cmdl
    incls = "#include \"feldspar_c99.h\"\n\n"

-- | Compiler core.
-- Everything should call this function and only do a trivial interface adaptation.
-- Do not duplicate.
compileToCCore :: Syntactic c => String -> Options -> c -> SplitModule
compileToCCore n opts p = fromMaybe err $ snd p'
  where err = error "compileToCCore: translate failed"
        p' = translate opts{functionName = n} p

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x,y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance Pretty Module where
  pretty m = compToCWithInfos defaultOptions m

instance Pretty SplitModule where
  pretty (SplitModule impl intf) = "// Interface\n" ++ sourceCode intf ++
                                   "\n// Implementation\n" ++ sourceCode impl

instance (Pretty a, Pretty b) => Pretty (Either a b) where
  pretty = either pretty pretty

codegen :: Options
        -> Prog (Either Module
                (Either (Module, Module) SplitModule)) Int
        -> Prog SplitModule Int
codegen opts
  | "c" == codeGenerator (platform opts)
  = passT ctrl BPCompile  (either (compileSplitModule opts) id)
  . passT ctrl BPSplit    (either (Left . splitModule) id)
  | otherwise
  = error $ "Compiler.codegen: unknown code generator " ++
             codeGenerator (platform opts)
  where ctrl = passCtrl opts
