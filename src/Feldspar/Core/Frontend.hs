{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
-- Orphans from the QuickCheck instances.
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Feldspar.Core.Frontend
    ( module Data.Patch
    , Syntactic
    , Syntax
    , Internal

    , Data

    , Pass(..)
    , frontend

    , module Feldspar.Core.Language

    -- * Options
    , Options(..)
    , defaultOptions
    , c99PlatformOptions
    , c99OpenMpPlatformOptions
    , c99WoolPlatformOptions
    , tic64xPlatformOptions
    , Target(..)

    -- * Interactive commands
    , showExpr
    , showUntyped
    , showUntyped'
    , printExpr
    , printExpr2
    , printExpr2With
    , drawUntyped
    , drawUntypedWith
    , showAST
    , drawAST
    , writeHtmlAST
    , showDecor
    , drawDecor
    , eval

    -- * Language utilities
    , desugar
    , sugar
    , resugar
    , value

    -- * QuickCheck
    , (===>)
    , (====)

    -- * Type constraints
    , tData
    , tArr1
    , tArr2

    -- * Functions
    , ilog2
    , nlz
    ) where

import Prelude as P

import Test.QuickCheck

import Data.Patch
import Data.Tree (Tree)
import Data.Tree.View
import Data.Hash (Hashable)

import Feldspar.Core.Reify hiding (desugar, sugar)
import qualified Feldspar.Core.Eval as E

import Feldspar.Compiler (frontend, reifyFeld, renameExp, translate)
import Feldspar.Compiler.Options (Options(..), Pass(..),
                                  PassCtrl(..), Target(..),
                                  c99OpenMpPlatformOptions,
                                  c99PlatformOptions,
                                  c99WoolPlatformOptions,
                                  defaultOptions,
                                  tic64xPlatformOptions)
import Feldspar.Core.AdjustBindings (adjustBindings)
import Feldspar.Core.Middleend.CreateTasks
import Feldspar.Core.Middleend.FromTyped (toU)
import Feldspar.Core.Middleend.LetSinking
import Feldspar.Core.Middleend.OptimizeUntyped
import Feldspar.Core.Middleend.PushLets
import Feldspar.Core.Middleend.UniqueVars
import qualified Feldspar.Core.SizeProp as SP
import Feldspar.Core.Types
import Feldspar.Core.UntypedRepresentation (UntypedFeld,
                                            stringTree, stringTreeExp
                                           )
import Feldspar.Core.Language
import Feldspar.Core.ValueInfo (ValueInfo)

stringTreeASTF :: ASTF a -> Tree String
stringTreeASTF = stringTree . untypeUnOpt defaultOptions

showDecorWith :: (ValueInfo -> String) -> ASTF a -> String
showDecorWith f = showTree . stringTreeExp g . untypeDecor defaultOptions
  where g x = " in " ++ f x

showExpr :: Syntactic a => a -> String
showExpr = showUntyped' FPUnASTF defaultOptions

-- | Show an untyped expression
showUntyped :: Syntactic a => Options -> a -> String
showUntyped opts prg = head . fst $ translate opts' prg
  where opts' = opts{passCtrl = (passCtrl opts){ wrBefore = [FPCreateTasks]
                                               , stopBefore = [FPCreateTasks]}}

-- | Show an expression after a specific frontend pass
showUntyped' :: Syntactic a => Pass -> Options -> a -> String
showUntyped' p opts prg = head . fst $ translate opts' prg
  where opts' = opts{passCtrl = (passCtrl opts){wrAfter = [p], stopAfter = [p]}}

-- | Print an optimized untyped expression
printExpr2 :: Syntactic a => a -> IO ()
printExpr2 = printExpr2With defaultOptions

-- | Draw the untyped syntax tree using unicode art
drawUntyped :: Syntactic a => a -> IO ()
drawUntyped = drawUntypedWith defaultOptions

-- | Draw the untyped syntax tree using unicode art
drawUntypedWith :: Syntactic a => Options -> a -> IO ()
drawUntypedWith opts = drawTree . stringTree . untype opts . reifyFeld

-- | Print an optimized expression
printExpr :: Syntactic a => a -> IO ()
printExpr = print . showUntyped' FPUnASTF defaultOptions

-- | Print an optimized untyped expression with options
printExpr2With :: Syntactic a => Options -> a -> IO ()
printExpr2With opts = print . untype opts . reifyFeld

-- | Show the syntax tree using Unicode art
showAST :: Syntactic a => a -> String
showAST = showTree . stringTreeASTF . reifyFeld

-- | Draw the syntax tree on the terminal using Unicode art
drawAST :: Syntactic a => a -> IO ()
drawAST = putStrLn . showAST

-- | Write the syntax tree to an HTML file with foldable nodes
writeHtmlAST :: Syntactic a => FilePath -> a -> IO ()
writeHtmlAST file = writeHtmlTree Nothing file
                  . fmap (\n -> NodeInfo InitiallyExpanded n "")
                  . stringTreeASTF
                  . reifyFeld

-- | Draw a syntax tree decorated with type and size information
showDecor :: Syntactic a => a -> String
showDecor = showDecorWith show . reifyFeld

-- | Draw a syntax tree decorated with type and size information
drawDecor :: Syntactic a => a -> IO ()
drawDecor = putStrLn . showDecor

eval :: Syntactic a => a -> Internal a
eval = E.eval . unASTF . reifyFeld

desugar :: Syntactic a => a -> Data (Internal a)
desugar = resugar

sugar :: Syntactic a => Data (Internal a) -> a
sugar = resugar

-- * Old compatibility functions

-- FIXME: Replace the calls to the compatibility functions
--        with calls to the frontend function.

-- | Untype, optimize and unannotate.
untype :: Options -> ASTF a -> UntypedFeld ValueInfo
untype opts = cleanUp opts
            . untypeDecor opts

-- | Untype and optimize.
untypeDecor :: Options -> ASTF a -> UntypedFeld ValueInfo
untypeDecor opts = pushLets
                 . optimize
                 . sinkLets opts
                 . justUntype

-- | External module interface.
untypeUnOpt :: Options -> ASTF a -> UntypedFeld ValueInfo
untypeUnOpt opts = cleanUp opts
                 . justUntype

-- | Only do the conversion to UntypedFeld ValueInfo
justUntype :: ASTF a -> UntypedFeld ValueInfo
justUntype = renameExp . toU . SP.sizeProp . adjustBindings . unASTF

-- | Prepare the code for fromCore
cleanUp :: Options -> UntypedFeld ValueInfo -> UntypedFeld ValueInfo
cleanUp opts = createTasks opts . uniqueVars

--------------------------------------------------------------------------------
-- * QuickCheck
--------------------------------------------------------------------------------

instance (Type a, Arbitrary a, Hashable a) => Arbitrary (Data a)
  where
    arbitrary = fmap value arbitrary

instance Testable (Data Bool)
  where
    property = property . eval

(===>) :: Testable prop => Data Bool -> prop -> Property
a ===> b = eval a ==> b


-- | Test that two function of the same arity have the same semantics
class Equal a
  where
    (====) :: a -> a -> Property

instance {-# OVERLAPPABLE #-} (P.Eq a, Show a) => Equal a
  where
    x ==== y = x === y

instance (Show a, Arbitrary a, Equal b) => Equal (a -> b)
  where
    f ==== g = property (\x -> f x ==== g x)


--------------------------------------------------------------------------------
-- * Type annotations
--------------------------------------------------------------------------------

tData :: Patch a a -> Patch (Data a) (Data a)
tData _ = id

tArr1 :: Patch a a -> Patch (Data [a]) (Data [a])
tArr1 _ = id

tArr2 :: Patch a a -> Patch (Data [[a]]) (Data [[a]])
tArr2 _ = id


--------------------------------------------------------------------------------
-- * Functions
--------------------------------------------------------------------------------

-- | Integer logarithm in base 2
--   Based on an algorithm in Hacker's Delight
ilog2 :: (Bits a) => Data a -> Data Index
ilog2 x = bitSize x - 1 - nlz x

-- | Count leading zeros
--   Based on an algorithm in Hacker's Delight
nlz :: (Bits a) => Data a -> Data Index
nlz x = bitCount $ complement $ foldl go x $ takeWhile (P.< bitSize' x) $ P.map (2 P.^) [(0::Integer)..]
  where
    go b s = share b $ \b' -> b' .|. (b' .>>. value s)
      -- TODO share is probably not needed when observable sharing is implemented
