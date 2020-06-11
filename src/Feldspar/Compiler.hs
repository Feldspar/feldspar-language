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
  ) where

import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Compiler
import Feldspar.Compiler.Frontend.Interactive.Interface
import Feldspar.Core.Interpretation (FeldOpts(..), Target(..))
