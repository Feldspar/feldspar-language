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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module Feldspar.Compiler.Frontend.Interactive.Interface where

import Feldspar.Core.Frontend (Syntactic, reifyFeld)
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))
import Feldspar.Core.UntypedRepresentation (UntypedFeld)
import Feldspar.Core.Middleend.PassManager
import Feldspar.Core.Middleend.FromTyped (FrontendPass, frontend)
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Imperative.FromCore
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Library
import Feldspar.Compiler.Backend.C.Platforms (availablePlatforms, platformFromName)
import Feldspar.Compiler.Imperative.Representation (Module(..))

import Data.Char
import Control.Monad (when, unless)
import System.FilePath (takeFileName)
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.IO

-- ================================================================================================
--  == Interactive compilation
-- ================================================================================================

compile :: (Syntactic t) => t -> FilePath -> String -> Options -> IO ()
compile prg fileName funName opts = writeFiles compRes fileName (codeGenerator $ platform opts)
  where compRes = compileToCCore funName opts prg

compileUT :: UntypedFeld -> FilePath -> String -> Options -> IO ()
compileUT prg fileName funName opts = writeFiles compRes fileName (codeGenerator $ platform opts)
  where compRes = compileToCCore' opts prg'
        prg'    = fst $ fromCoreUT opts (encodeFunctionName funName) prg

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

icompile :: (Syntactic t) => t -> IO ()
icompile = icompileWith defaultOptions

icompileWith :: (Syntactic t) => Options -> t -> IO ()
icompileWith opts = icompile' opts "test"

icompile' :: (Syntactic t) => Options -> String -> t -> IO ()
icompile' opts fName prg = do
    let res = compileToCCore fName opts prg
    when (printHeader opts) $ do
      putStrLn "=============== Header ================"
      putStrLn $ sourceCode $ interface res
      putStrLn "=============== Source ================"
    putStrLn $ sourceCode $ implementation res

-- | Get the generated core for a program.
getCore :: (Syntactic t) => t -> Module ()
getCore = fromCore defaultOptions "test"

-- | Print the generated core for a program.
printCore :: (Syntactic t) => t -> IO ()
printCore prog = print $ getCore prog

targetsFromPlatform :: Platform -> [Target]
targetsFromPlatform pf = tfp $ name pf
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
  let (opts1,nonopts) = decodeOpts (optsFromName opts name') args
  let header = "Usage: " ++ name' ++ " <option>...\n"
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

outFileNames :: ProgOpts -> [String]
outFileNames opts = [name' ++ ".h", name' ++ ".c"]
  where name' = outFileName opts

decodeOpts :: ProgOpts -> [String] -> (ProgOpts, [String])
decodeOpts optsIn argv
  | null errors = (foldl (\ o f -> f o) optsIn actions, nonOptions)
  | otherwise = error $ unlines errors
   where (actions, nonOptions, errors) = getOpt Permute optionDescs argv

passInfo :: String
passInfo = "\nPASS is a frontend pass from\n" ++
           unlines (map ((++) "  " . unwords) $ chunksOf 5 $ map show [minBound .. maxBound :: FrontendPass]) ++
           "or a backend pass from\n" ++
           unlines (map ((++) "  " . unwords) $ chunksOf 5 $ map show [minBound .. maxBound :: BackendPass])
  where chunksOf _ [] = []
        chunksOf n xs = take n xs:chunksOf n (drop n xs)

targetInfo :: String
targetInfo = "\nTARGET is one of " ++ unwords (map name availablePlatforms) ++ "\n\n"

optionDescs :: [OptDescr (ProgOpts -> ProgOpts)]
optionDescs = driverOpts

driverOpts :: [OptDescr (ProgOpts -> ProgOpts)]
driverOpts =
  [ Option []  ["writeBefore"] (ReqArg (chooseEnd addWrBefore) "PASS")   "write IR before PASS"
  , Option []  ["writeAfter"]  (ReqArg (chooseEnd addWrAfter) "PASS")    "write IR after PASS"
  , Option []  ["stopBefore"]  (ReqArg (chooseEnd setStopBefore) "PASS") "stop processing before PASS"
  , Option []  ["stopAfter"]   (ReqArg (chooseEnd setStopAfter) "PASS")  "stop processing after PASS"
  , Option []  ["skip"]        (ReqArg (chooseEnd addSkip) "PASS")       "skip PASS"
  , Option "o" ["outFile"]     (ReqArg (\ arg opts -> opts{outFileName = arg}) "FILE") "set base name of out file"
  , Option "p" ["passFile"]    (ReqArg (\ arg opts -> opts{passFileName = arg}) "FILE") "set name of pass file"
  , Option []  ["funcName"]    (ReqArg (\ arg opts -> opts{functionName = arg}) "IDENTIFIER") "set name of generated function"
  , Option "t" ["target"]      (ReqArg (\ arg opts -> setTarget opts arg) "TARGET") "set target"
  , Option "h" ["help"]        (NoArg (\ opts -> opts{printHelp = True})) "print a useage message"
  ]

setTarget :: ProgOpts -> String -> ProgOpts
setTarget opts str = opts{backOpts = bopts{platform = pf,
                                           frontendOpts = fopts{targets = targetsFromPlatform pf}}}
   where bopts = backOpts opts
         fopts = frontendOpts bopts
         pf = platformFromName str

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
  where (ssf, ut) = frontend (frontendCtrl opts) fopts $ reifyFeld p
        (ssb, as) = maybe ([], Nothing) (backend (backendCtrl opts) bopts name') ut
        bopts     = backOpts opts
        fopts     = frontendOpts bopts
        name'     = functionName opts
