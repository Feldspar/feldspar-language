{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

    , module Feldspar.Core.Language

    , FeldOpts
    , defaultFeldOpts
    , reifyFeld
    , reifyFeldUnOpt
    , showExpr
    , printExpr
    , printExpr2
    , printExprWith
    , printExpr2With
    , printExprUnOpt
    , drawUntyped
    , drawUntypedWith
    , showAST
    , drawAST
    , drawASTUnOpt
    , writeHtmlAST
    , showDecor
    , drawDecor
    , eval
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

import qualified Feldspar.Core.Reify as Syntactic
import Feldspar.Core.Reify hiding (desugar, sugar)
import qualified Feldspar.Core.Eval as E

import Feldspar.Core.Types
import Feldspar.Core.Interpretation (FeldOpts, defaultFeldOpts)
import Feldspar.Core.Middleend.FromTyped (untype, untypeUnOpt, untypeDecor)
import Feldspar.Core.UntypedRepresentation (VarId, stringTree, stringTreeExp)
import Feldspar.Core.Language
import Feldspar.ValueInfo (ValueInfo)

reifyFeld :: Syntactic a => a -> ASTF (Internal a)
reifyFeld = Syntactic.desugar

reifyFeldUnOpt :: Syntactic a => a -> ASTF (Internal a)
reifyFeldUnOpt = reifyFeld

stringTreeASTF :: ASTF a -> Tree String
stringTreeASTF = stringTree . untypeUnOpt defaultFeldOpts

class StringTree (a :: * -> *) where

showDecorWith :: (ValueInfo -> String) -> ASTF a -> String
showDecorWith f = showTree . stringTreeExp g . untypeDecor defaultFeldOpts
  where g x = " in " ++ f x

showExpr :: Syntactic a => a -> String
showExpr = render . reifyFeld

-- | Print an optimized untyped expression
printExpr2 :: Syntactic a => a -> IO ()
printExpr2 = printExpr2With defaultFeldOpts

-- | Draw the untyped syntax tree using unicode art
drawUntyped :: Syntactic a => a -> IO ()
drawUntyped = drawUntypedWith defaultFeldOpts

-- | Draw the untyped syntax tree using unicode art
drawUntypedWith :: Syntactic a => FeldOpts -> a -> IO ()
drawUntypedWith opts = drawTree . stringTree . untype opts . reifyFeld

-- | Print an optimized expression
printExpr :: Syntactic a => a -> IO ()
printExpr = print . reifyFeld

-- | Print an optimized untyped expression with options
printExpr2With :: Syntactic a => FeldOpts -> a -> IO ()
printExpr2With opts = print . untype opts . reifyFeld

-- | Print an optimized expression with options
printExprWith :: Syntactic a => FeldOpts -> a -> IO ()
printExprWith opts = print . reifyFeld

-- | Print an unoptimized expression
printExprUnOpt :: Syntactic a => a -> IO ()
printExprUnOpt = print . reifyFeldUnOpt

-- | Show the syntax tree using Unicode art
showAST :: Syntactic a => a -> String
showAST = showTree . stringTreeASTF . reifyFeld

-- | Draw the syntax tree on the terminal using Unicode art
drawAST :: Syntactic a => a -> IO ()
drawAST = putStrLn . showAST . reifyFeld

drawASTUnOpt :: Syntactic a => a -> IO ()
drawASTUnOpt = putStrLn . showAST . reifyFeldUnOpt

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
drawDecor = putStrLn . showDecorWith show . reifyFeld

eval :: Syntactic a => a -> Internal a
eval = E.eval . unASTF . reifyFeld

desugar :: Syntactic a => a -> Data (Internal a)
desugar = resugar

sugar :: Syntactic a => Data (Internal a) -> a
sugar = resugar

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
