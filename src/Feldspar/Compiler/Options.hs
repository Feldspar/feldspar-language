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
  , c99PlatformOptions
  , c99OpenMpPlatformOptions
  , c99WoolPlatformOptions
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
data Target = Wool | BA
  deriving (Eq, Lift, Show)

-- | Decide whether a Target is enabled in Options.
inTarget :: Target -> Options -> Bool
inTarget t opts = t `elem` (targets . platform $ opts)

data Options = Options
  { platform          :: Platform
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

c99WoolPlatformOptions :: Options
c99WoolPlatformOptions          = defaultOptions { platform = c99Wool }

tic64xPlatformOptions :: Options
tic64xPlatformOptions           = defaultOptions { platform = tic64x }

data Platform = Platform {
  platformName    :: String,    -- ^ Name of the platform
  targets         :: [Target],  -- ^ Targets for the platform
  platformRenames :: [Rename],  -- ^ Renames of functions/operators
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
    targets       = [],
    platformRenames = c99list,
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
              , targets = Wool:targets c99
              , compilerFlags = "-DUSE_WOOL":compilerFlags c99
              , varFloating = False
              }

ba :: Platform
ba = c99 { platformName = "ba"
         , targets = BA:targets c99
         , codeGenerator = "ba"
         }

tic64x :: Platform
tic64x = c99 { platformName = "tic64x"
             , platformRenames = tic64xlist ++ platformRenames c99
             , compilerFlags = "-D__TIC64X__":compilerFlags c99
             }

-- * Renamer data types to avoid cyclic imports.
type Rename = (String, [(Which, Destination)])

data Predicate = Complex | Float | Signed32 | Unsigned32
  deriving (Lift, Show)

data Which = All | Only Predicate
  deriving (Lift, Show)

data WhichType = FunType | ArgType
  deriving (Lift, Show)

data Destination =
    Name String
  | Extend WhichType
  | ExtendRename WhichType String
   deriving (Lift, Show)

-- A rename is the name of the function to be renamed coupled with a
-- list of preconditions for renaming to happen and a the destination
-- name if the precondition is held. First match is executed and the
-- destination name becomes whatever the template specifies.

-- | C99 renaming list.
c99list :: [Rename]
c99list =
  [ ("/=",            [ (All, Name "!=")])
  , ("not",           [ (All, Name "!")])
  , ("quot",          [ (All, Name "/")])
  , ("rem",           [ (All, Name "%")])
  , (".&.",           [ (All, Name "&")])
  , (".|.",           [ (All, Name "|")])
  , ("xor",           [ (All, Name "^")])
  , ("complement",    [ (All, Name "~")])
  , ("shiftL",        [ (All, Name "<<")])
  , ("shiftLU",       [ (All, Name "<<")])
  , ("shiftR",        [ (All, Name ">>")])
  , ("shiftRU",       [ (All, Name ">>")])
  , ("creal",         [ (All, Name "crealf")])
  , ("cimag",         [ (All, Name "cimagf")])
  , ("conjugate",     [ (All, Name "conjf")])
  , ("magnitude",     [ (All, Name "cabsf")])
  , ("phase",         [ (All, Name "cargf")])
  , ("atan2",         [ (Only Complex, Name "atan2f") ])
  ] ++
  map mkC99TrigRule ["exp", "sqrt", "log", "**", "sin", "tan", "cos", "asin"
                    , "atan", "acos", "sinh", "tanh", "cosh", "asinh", "atanh"
                    , "acosh"] ++
  -- Extend these functions based on the function type.
  map (mkC99ExtendRule FunType) [ "abs", "signum", "logBase", "setBit", "clearBit"
                                , "complementBit", "rotateL", "rotateR"
                                , "reverseBits" ] ++
  -- Extend these functions based on the argument type.
  map (mkC99ExtendRule ArgType) [ "testBit", "bitScan", "bitCount", "complex"
                                 , "mkPolar", "cis"]

-- | Make C99 extend rule.
mkC99ExtendRule :: WhichType -> String -> Rename
mkC99ExtendRule t s = (s, [ (All, Extend t) ])

-- | Make C99 trig rule.
mkC99TrigRule :: String -> Rename
mkC99TrigRule s = (s, [ (Only Complex, Name ('c':s')), (All, Name s') ])
  where s' = s ++ "f"

-- | Tic64x renaming list.
tic64xlist :: [Rename]
tic64xlist =
  [ ("==",          [ (Only Complex, ExtendRename ArgType "equal") ])
  , ("abs",         [ (Only Float, Name "_fabs"), (Only Signed32, Name "_abs") ])
  , ("+",           [ (Only Complex, ExtendRename ArgType "add") ])
  , ("-",           [ (Only Complex, ExtendRename ArgType "sub") ])
  , ("*",           [ (Only Complex, ExtendRename ArgType "mult") ])
  , ("/",           [ (Only Complex, ExtendRename ArgType "div") ])
  ] ++
  map mkTic64xComplexRule ["exp", "sqrt", "log", "sin", "tan", "cos", "asin"
                          ,"atan", "acos", "sinh", "tanh", "cosh", "asinh"
                          ,"atanh","acosh","creal","cimag", "conjugate"
                          ,"magnitude","phase", "logBase"] ++
  [ ("**",          [ (Only Complex, ExtendRename ArgType "cpow") ])
  , ("rotateL",     [ (Only Unsigned32, Name "_rotl") ])
  , ("reverseBits", [ (Only Unsigned32, Name "_bitr") ])
  ]

-- | Create Tic64x rule for complex type.
mkTic64xComplexRule :: String -> Rename
mkTic64xComplexRule s = (s, [ (Only Complex, Extend ArgType) ] )
