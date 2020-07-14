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
{-# LANGUAGE DeriveLift #-}
{-# OPTIONS_GHC -Wall #-}

-- | Option handling data structures for Feldspar
module Feldspar.Compiler.Options
  ( ErrorClass(..)
  , handleError
  , encodeFunctionName
  , Pretty(..)
  , PassCtrl(..)
  , Pass(..)
  , Target(..)
  , inTarget
  , Options(..)
  , defaultOptions
  , sicsOptions
  , sicsOptions2
  , sicsOptions3
  , c99PlatformOptions
  , c99OpenMpPlatformOptions
  , tic64xPlatformOptions
  , Platform(..)
  , availablePlatforms
  , platformFromName
  , c99
  , c99OpenMp
  , c99Wool
  , ba
  , tic64x
  , Rename
  , Predicate(..)
  , Which(..)
  , WhichType(..)
  , Destination(..)
  ) where

import Data.List (isPrefixOf)
import Language.Haskell.TH.Syntax (Lift(..))

-- * Error handling utils.

-- | Error categorization
data ErrorClass
  = InvariantViolation
  | InternalError
  | Warning
  deriving (Eq, Show)

handleError :: String -> ErrorClass -> String -> a
handleError place errorClass message =
  error $ "[" ++ show errorClass ++ " @ " ++ place ++ "]: " ++ message

-- * String utils

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl
  | find `isPrefixOf` s = repl ++ replace (drop (length find) s) find repl
  | otherwise = head s : replace (tail s) find repl

-- | Encode special characters in function names.
encodeFunctionName :: String -> String
encodeFunctionName fName = replace (replace fName "_" "__") "'" "_prime"

-- * Pretty printing utils.

-- | Class for things that can be pretty printed.
class Pretty a where
  pretty :: a -> String

-- * Option handling data structures and utils.

-- | Record controlling pass behavior
data PassCtrl = PassCtrl
  { wrBefore   :: [Pass] -- ^ Write IR before these passes
  , wrAfter    :: [Pass] -- ^ Write IR after these passes
  , stopBefore :: [Pass] -- ^ Stop before these passes
  , stopAfter  :: [Pass] -- ^ Stop after these passes
  , skip       :: [Pass] -- ^ Skip these passes
  } deriving (Lift, Show)

-- | Enumeration of compiler passes
data Pass
  = FPUnASTF
  | FPAdjustBind
  | FPSizeProp
  | FPUntype
  | FPRename
  | FPSinkLets
  | FPOptimize
  | FPPushLets
  | FPExpand
  | FPUnique
  | FPUnAnnotate
  | FPCreateTasks
  | BPFromCore
  | BPArrayOps
  | BPRename
  | BPAdapt
  | BPSplit
  | BPCompile
  | BPUnsplit
  deriving (Bounded, Enum, Eq, Lift, Read, Show)

-- | Possible compilation targets in a broad sense.
data Target = RegionInf | Wool | CSE | SICS | BA
  deriving (Eq, Lift)

-- | Decide whether a Target is enabled in Options.
inTarget :: Target -> Options -> Bool
inTarget t opts = t `elem` targets opts

data Options = Options
  { platform          :: Platform
  , targets           :: [Target]
  , printHeader       :: Bool
  , useNativeArrays   :: Bool
  , useNativeReturns  :: Bool     -- ^ Should the generated function return by value or by
                                  --   reference (fast return)? This option will be ignored for
                                  --   types that can't be fast-returned.
  , safetyLimit       :: Integer  -- ^ Threshold to stop when the size information gets lost.
  , nestSize          :: Int      -- ^ Indentation size for PrettyPrinting
  , passFileName      :: String   -- ^ Filename for debug output
  , outFileName       :: String   -- ^ Filename for output
  , functionName      :: String   -- ^ Function name to generate
  , passCtrl          :: PassCtrl -- ^ Pass configuration
  , printHelp         :: Bool     -- ^ Print help
  } deriving Lift

-- | Predefined options
defaultOptions :: Options
defaultOptions
    = Options
    { platform          = c99
    , targets           = []
    , printHeader       = False
    , useNativeArrays   = False
    , useNativeReturns  = False
    , safetyLimit       = 2000
    , nestSize          = 2
    , passFileName      = ""
    , outFileName       = ""
    , functionName      = ""
    , passCtrl          = PassCtrl [] [] [] [] []
    , printHelp         = False
    }

c99PlatformOptions :: Options
c99PlatformOptions              = defaultOptions

c99OpenMpPlatformOptions :: Options
c99OpenMpPlatformOptions        = defaultOptions { platform = c99OpenMp }

tic64xPlatformOptions :: Options
tic64xPlatformOptions           = defaultOptions { platform = tic64x }

sicsOptions :: Options
sicsOptions = defaultOptions { targets = [SICS,CSE] }

sicsOptions2 :: Options
sicsOptions2 = defaultOptions { targets = [SICS] }

sicsOptions3 :: Options
sicsOptions3 = defaultOptions { platform = c99Wool, targets = [SICS,CSE,Wool] }

data Platform = Platform {
  platformName    :: String,    -- ^ Name of the platform
  compilerFlags   :: [String],  -- ^ Flags to pass to the C compiler
  varFloating     :: Bool,      -- ^ Declare variables on the top level scope
  codeGenerator   :: String     -- ^ Name of the code generator
} deriving (Lift, Show)

availablePlatforms :: [Platform]
availablePlatforms = [ c99, c99OpenMp, c99Wool, ba, tic64x ]

platformFromName :: String -> Platform
platformFromName str
  = head $ [pf | pf <- availablePlatforms, platformName pf == str]
             ++ error ("platformFromName: No platform named " ++ str)

c99 :: Platform
c99 = Platform {
    platformName = "c99",
    compilerFlags = ["-std=c99", "-Wall"
                    ,"-D_XOPEN_SOURCE" -- Required for M_PI in math.h
                    ],
    varFloating = True,
    codeGenerator = "c"
}

c99OpenMp :: Platform
c99OpenMp = c99 { platformName = "c99OpenMp"
                , varFloating = False
                }

c99Wool :: Platform
c99Wool = c99 { platformName = "c99Wool"
              , compilerFlags = "-DUSE_WOOL":compilerFlags c99
              , varFloating = False
              }

ba :: Platform
ba = c99 { platformName = "ba"
         , codeGenerator = "ba"
         }

tic64x :: Platform
tic64x = c99 { platformName = "tic64x"
             , compilerFlags = "-D__TIC64X__":compilerFlags c99
             }

-- * Renamer data types to avoid cyclic imports.
type Rename = (String, [(Which, Destination)])

data Predicate = Complex | Float | Signed32 | Unsigned32
  deriving Show

data Which = All | Only Predicate
  deriving Show

data WhichType = FunType | ArgType
  deriving Show

data Destination =
    Name String
  | Extend WhichType Platform
  | ExtendRename WhichType Platform String
   deriving Show
