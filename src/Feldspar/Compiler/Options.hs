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
  , Pretty(..)
  , Target(..)
  , FeldOpts(..)
  , defaultFeldOpts
  , inTarget
  , Options(..)
  , Platform(..)
  , Rename
  , Predicate(..)
  , Which(..)
  , WhichType(..)
  , Destination(..)
  ) where

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

-- * Pretty printing utils.

-- | Class for things that can be pretty printed.
class Pretty a where
  pretty :: a -> String

-- * Option handling data structures and utils.

-- | Possible compilation targets in a broad sense.
data Target = RegionInf | Wool | CSE | SICS | BA
  deriving (Eq, Lift)

-- | A record with options for explicit passing in rewrite rules.
data FeldOpts = FeldOpts
    { targets    :: [Target]
    } deriving Lift

-- | Default options.
defaultFeldOpts :: FeldOpts
defaultFeldOpts = FeldOpts { targets = [] }

-- | Decide whether a Target is enabled in FeldOpts.
inTarget :: Target -> FeldOpts -> Bool
inTarget t opts = t `elem` targets opts

data Options = Options
  { platform          :: Platform
  , printHeader       :: Bool
  , useNativeArrays   :: Bool
  , useNativeReturns  :: Bool     -- ^ Should the generated function return by value or by
                                  --   reference (fast return)? This option will be ignored for
                                  --   types that can't be fast-returned.
  , frontendOpts      :: FeldOpts -- ^ Options for the front end optimization chain
  , safetyLimit       :: Integer  -- ^ Threshold to stop when the size information gets lost.
  , nestSize          :: Int      -- ^ Indentation size for PrettyPrinting
  } deriving Lift

data Platform = Platform {
  platformName    :: String,
  includes        :: [String],
  varFloating     :: Bool,
  codeGenerator   :: String
} deriving (Lift, Show)

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
