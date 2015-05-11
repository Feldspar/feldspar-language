module Feldspar.Core.Frontend.LoopM where

import Language.Syntactic

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Loop

import Feldspar.Core.Frontend.Mutable

forM :: (Syntax a) => Data Length -> (Data Index -> M a) -> M ()
forM = sugarSymC For

whileM :: Syntax a => M (Data Bool) -> M a -> M ()
whileM = sugarSymC While


