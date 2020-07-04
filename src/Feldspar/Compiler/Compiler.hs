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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-- FIXME: Move the Pretty instances to the right place.
{-# OPTIONS_GHC -Wno-orphans #-}

module Feldspar.Compiler.Compiler (
    compileToCCore
  , compileToCCore'
  , defaultOptions
  , sicsOptions
  , sicsOptions2
  , sicsOptions3
  , c99PlatformOptions
  , c99OpenMpPlatformOptions
  , tic64xPlatformOptions
  , SplitModule(..)
  , CompiledModule(..)
  , BackendPass(..)
  , backend
  , fromCore
  , ProgOpts(..)
  , defaultProgOpts
  ) where

import Data.List (partition)
import Data.Maybe (fromMaybe)

import Feldspar.Core.Frontend (FrontendPass, Syntactic, frontend, reifyFeld)
import Feldspar.Core.UntypedRepresentation (UntypedFeld, VarId)
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

instance Pretty VarId where
  pretty v = show v

instance Pretty SplitModule where
  pretty (SplitModule impl intf) = "// Interface\n" ++ sourceCode intf ++
                                   "\n// Implementation\n" ++ sourceCode impl

backend :: PassCtrl BackendPass -> Options -> String -> UntypedFeld -> ([String], Maybe SplitModule)
backend ctrl opts name = evalPasses 0
                       $ codegen (codeGenerator $ platform opts) ctrl opts
                       . pc BPRename   (rename opts False)
                       . pc BPArrayOps (arrayOps opts)
                       . pt BPFromCore (fst . fromCoreUT opts (encodeFunctionName name))
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
  | Just prg <- snd $ frontend ctrl (frontendOpts opt) $ reifyFeld prog
  = fst $ fromCoreUT opt funname prg
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
