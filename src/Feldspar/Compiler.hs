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
  ( compile
  , compileUT
  , icompile
  , icompileWith
  , icompile'
  , program
  , programOpts
  , programOptsArgs
  , getCore
  , printCore
  , Options(..)
  , defaultOptions
  , sicsOptions
  , sicsOptions2
  , sicsOptions3
  , FeldOpts(..)
  , Target(..)
  , c99PlatformOptions
  , c99OpenMpPlatformOptions
  , tic64xPlatformOptions
  -- * Internal functions
  , compileToCCore'
  , writeFiles
  , sourceCode
  , CompiledModule
  , SplitModule(..)
  ) where

import Control.Monad (when, unless)
import Data.Char (toUpper)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..),
                              getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.FilePath (takeFileName)
import System.IO (BufferMode(..), IOMode(..), hClose, hPutStr,
                  hSetBuffering, openFile)

import Feldspar.Core.Frontend (FrontendPass, Syntactic, frontend)
import Feldspar.Core.UntypedRepresentation (UntypedFeld)
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Backend.C.Platforms
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.MachineLowering
import Feldspar.Compiler.Backend.C.Tic64x
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Imperative.ArrayOps
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Options
import Feldspar.Core.Middleend.PassManager

compile :: Syntactic t => t -> FilePath -> String -> Options -> IO ()
compile prg fileName funName opts = writeFiles compRes fileName (codeGenerator $ platform opts)
  where compRes = compileToCCore funName opts prg

compileUT :: UntypedFeld -> FilePath -> String -> Options -> IO ()
compileUT prg fileName funName opts = writeFiles compRes fileName (codeGenerator $ platform opts)
  where compRes = compileToCCore' opts prg'
        prg'    = fromCoreUT opts (encodeFunctionName funName) prg

writeFiles :: SplitModule -> FilePath -> String -> IO ()
writeFiles prg fileName "c" = do
    writeFile cfile $ unlines [ "#include \"" ++ takeFileName hfile ++ "\""
                              , "\n"
                              , sourceCode $ implementation prg
                              ]
    writeFile hfile $ withIncludeGuard $ sourceCode $ interface prg
  where
    hfile = makeHFileName fileName
    cfile = makeCFileName fileName

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
writeFiles prg fileName _ = writeFile fileName $ sourceCode $ implementation prg

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

-- | Get the generated core for a program.
getCore :: Syntactic t => t -> Module ()
getCore = fromCore defaultOptions "test"

-- | Print the generated core for a program.
printCore :: Syntactic t => t -> IO ()
printCore prog = print $ getCore prog

targetsFromPlatform :: Platform -> [Target]
targetsFromPlatform = tfp . platformName
   where tfp "c99"       = []
         tfp "c99OpenMp" = []
         tfp "c99Wool"   = [Wool]
         tfp "ba"        = [BA]
         tfp p           = error $ "Interface.tfp: Unknown platform:" ++ p

program :: Syntactic a => a -> IO ()
program p = programOpts p defaultOptions

programOpts :: Syntactic a => a -> Options -> IO ()
programOpts p opts = do args <- getArgs
                        programOptsArgs p defaultProgOpts{backOpts = opts} args

programOptsArgs :: Syntactic a => a -> ProgOpts -> [String] -> IO ()
programOptsArgs p = programComp (const (return p))

programComp :: Syntactic a => ([String] -> IO a) -> ProgOpts -> [String] -> IO ()
programComp pc opts args = do
  name' <- getProgName
  let (opts1, nonopts) = decodeOpts (optsFromName opts name') args
      header = "Usage: " ++ name' ++ " <option>...\n"
                         ++ "where <option> is one of"
  if printHelp opts1
    then putStr $ usageInfo header optionDescs ++ passInfo ++ targetInfo
    else do
      p <- pc nonopts
      let (strs,mProgs) = translate opts1 p
      unless (null strs) $ writeFileLB (passFileName opts1) (concat strs)
      case mProgs of
        Nothing -> return ()
        Just prog -> writeFiles prog (outFileName opts1)
                      (codeGenerator $ platform $ backOpts opts1)

optsFromName :: ProgOpts -> String -> ProgOpts
optsFromName opts name' = opts{ passFileName = name' ++ ".passes"
                              , outFileName = name'
                              , functionName = name'}

decodeOpts :: ProgOpts -> [String] -> (ProgOpts, [String])
decodeOpts optsIn argv
  | null errors = (foldl (\o f -> f o) optsIn actions, nonOptions)
  | otherwise = error $ unlines errors
   where (actions, nonOptions, errors) = getOpt Permute optionDescs argv

passInfo :: String
passInfo = "\nPASS is a frontend pass from\n" ++
           unlines (map ((++) "  " . unwords) $ chunksOf 5 $ map show fps) ++
           "or a backend pass from\n" ++
           unlines (map ((++) "  " . unwords) $ chunksOf 5 $ map show bps)
  where chunksOf _ [] = []
        chunksOf n xs = take n xs:chunksOf n (drop n xs)
        fps = [minBound .. maxBound :: FrontendPass]
        bps = [minBound .. maxBound :: BackendPass]

targetInfo :: String
targetInfo = "\nTARGET is one of " ++ names ++ "\n\n"
  where names = unwords (map platformName availablePlatforms)

optionDescs :: [OptDescr (ProgOpts -> ProgOpts)]
optionDescs = driverOpts

driverOpts :: [OptDescr (ProgOpts -> ProgOpts)]
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

setTarget :: ProgOpts -> String -> ProgOpts
setTarget opts str
  = opts{backOpts = bopts{platform = pf, frontendOpts = fopts{targets = tgs}}}
   where bopts = backOpts opts
         fopts = frontendOpts bopts
         pf = platformFromName str
         tgs = targetsFromPlatform pf

chooseEnd :: (forall a . PassCtrl a -> a -> PassCtrl a) -> String -> ProgOpts -> ProgOpts
chooseEnd f str opts
  | [(p,_)] <- reads str = opts{frontendCtrl = f (frontendCtrl opts) p}
  | [(p,_)] <- reads str = opts{backendCtrl = f (backendCtrl opts) p}
  | otherwise = error $ "Compiler.chooseEnd: unrecognized pass " ++ str

writeFileLB :: String -> String -> IO ()
writeFileLB  "-"  str = putStr str
writeFileLB name' str = do fh <- openFile name' WriteMode
                           hSetBuffering fh LineBuffering
                           hPutStr fh str
                           hClose fh

translate :: Syntactic a => ProgOpts -> a -> ([String], Maybe SplitModule)
translate opts p = (ssf ++ ssb, as)
  where (ssf, ut) = frontend (frontendCtrl opts) fopts p
        (ssb, as) = maybe ([], Nothing) (backend (backendCtrl opts) bopts name') ut
        bopts     = backOpts opts
        fopts     = frontendOpts bopts
        name'     = functionName opts

data SplitModule = SplitModule
    { implementation :: CompiledModule
    , interface :: CompiledModule
    }

data CompiledModule = CompiledModule {
    sourceCode      :: String,
    debugModule     :: Module ()
}

-- | Split a module into interface and implemenation.
splitModule :: Module () -> (Module (), Module ())
splitModule m = (Module (hdr ++ createProcDecls (entities m)), Module body)
  where
    (hdr, body) = partition belongsToHeader (entities m)
    belongsToHeader :: Entity () -> Bool
    belongsToHeader StructDef{}                     = True
    belongsToHeader Proc{..} | Nothing <- procBody  = True
    belongsToHeader _                               = False
    -- TODO These only belongs in the header iff the types are used in a
    -- function interface
    createProcDecls :: [Entity ()] -> [Entity ()]
    createProcDecls = concatMap defToDecl
    defToDecl :: Entity () -> [Entity ()]
    defToDecl (Proc n False inp rtype _) = [Proc n False inp rtype Nothing]
    defToDecl _ = []

compileSplitModule :: Options -> (Module (), Module ()) -> SplitModule
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
    incls = genIncludeLines opts Nothing

-- | Compiler core.
-- Everything should call this function and only do a trivial interface adaptation.
-- Do not duplicate.
compileToCCore :: Syntactic c => String -> Options -> c -> SplitModule
compileToCCore name opts prg = compileToCCore' opts mod'
      where
        mod' = fromCore opts (encodeFunctionName name) prg

compileToCCore' :: Options -> Module () -> SplitModule
compileToCCore' opts m = compileSplitModule opts $ splitModule mod'
      where
        mod' = adaptTic64x opts $ rename opts False $ arrayOps opts m

genIncludeLines :: Options -> Maybe String -> String
genIncludeLines opts mainHeader = concatMap include incs ++ "\n\n"
  where
    include []            = ""
    include fname@('<':_) = "#include " ++ fname ++ "\n"
    include fname         = "#include \"" ++ fname ++ "\"\n"
    incs = includes (platform opts) ++ [fromMaybe "" mainHeader]

-- | Predefined options

defaultOptions :: Options
defaultOptions
    = Options
    { platform          = c99
    , printHeader       = False
    , useNativeArrays   = False
    , useNativeReturns  = False
    , frontendOpts      = defaultFeldOpts
    , safetyLimit       = 2000
    , nestSize          = 2
    }

c99PlatformOptions :: Options
c99PlatformOptions              = defaultOptions

c99OpenMpPlatformOptions :: Options
c99OpenMpPlatformOptions        = defaultOptions { platform = c99OpenMp }

tic64xPlatformOptions :: Options
tic64xPlatformOptions           = defaultOptions { platform = tic64x }

sicsOptions :: Options
sicsOptions = defaultOptions { frontendOpts = defaultFeldOpts { targets = [SICS,CSE] }}

sicsOptions2 :: Options
sicsOptions2 = defaultOptions { frontendOpts = defaultFeldOpts { targets = [SICS] }}

sicsOptions3 :: Options
sicsOptions3 = defaultOptions { platform = c99Wool, frontendOpts = defaultFeldOpts { targets = [SICS,CSE,Wool] }}

data BackendPass = BPFromCore
                 | BPArrayOps
                 | BPRename
                 | BPAdapt
                 | BPSplit
                 | BPCompile
                 | BPUnsplit
  deriving (Eq, Enum, Bounded, Read, Show)

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x,y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance Pretty (Module ()) where
  pretty m = compToCWithInfos defaultOptions m

instance Pretty SplitModule where
  pretty (SplitModule impl intf) = "// Interface\n" ++ sourceCode intf ++
                                   "\n// Implementation\n" ++ sourceCode impl

backend :: PassCtrl BackendPass -> Options -> String -> UntypedFeld -> ([String], Maybe SplitModule)
backend ctrl opts name = evalPasses 0
                       $ codegen (codeGenerator $ platform opts) ctrl opts
                       . pc BPRename   (rename opts False)
                       . pc BPArrayOps (arrayOps opts)
                       . pt BPFromCore (fromCoreUT opts (encodeFunctionName name))
  where pc :: Pretty a => BackendPass -> (a -> a) -> Prog a Int -> Prog a Int
        pc = passC ctrl
        pt :: (Pretty a, Pretty b) => BackendPass -> (a -> b) -> Prog a Int -> Prog b Int
        pt = passT ctrl

codegen :: String -> PassCtrl BackendPass -> Options -> Prog (Module ()) Int -> Prog SplitModule Int
codegen "c"   ctrl opts  = passT ctrl BPCompile  (compileSplitModule opts)
                         . passT ctrl BPSplit    splitModule
                         . passC ctrl BPAdapt    (adaptTic64x opts)
codegen gen   _    _     = error $ "Compiler.codegen: unknown code generator " ++ gen

-- | Get the generated core for an expression.
fromCore :: Syntactic a
    => Options
    -> String   -- ^ Name of the generated function
    -> a        -- ^ Expression to generate code for
    -> Module ()
fromCore opt funname prog
  | Just prg <- snd $ frontend ctrl (frontendOpts opt) prog
  = fromCoreUT opt funname prg
  | otherwise = error "fromCore: Internal error: frontend failed?"
   where ctrl = frontendCtrl defaultProgOpts

data ProgOpts =
    ProgOpts
    { backOpts     :: Options
    , passFileName :: String
    , outFileName  :: String
    , functionName :: String
    , frontendCtrl :: PassCtrl FrontendPass
    , backendCtrl  :: PassCtrl BackendPass
    , printHelp    :: Bool
    }

defaultProgOpts :: ProgOpts
defaultProgOpts =
    ProgOpts
    { backOpts     = defaultOptions
    , passFileName = ""
    , outFileName  = ""
    , functionName = ""
    , frontendCtrl = defaultPassCtrl
    , backendCtrl  = defaultPassCtrl
    , printHelp    = False
    }
