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

    , FrontendPass(..)
    , frontend

    , module Feldspar.Core.Language

    -- * Options
    , FeldOpts(..)
    , defaultFeldOpts
    , Options(..)
    , defaultOptions
    , sicsOptions
    , sicsOptions2
    , sicsOptions3
    , c99PlatformOptions
    , c99OpenMpPlatformOptions
    , tic64xPlatformOptions
    , Target(..)

    -- * Interactive commands
    , showExpr
    , showUntyped
    , showUntyped'
    , printExpr
    , printExpr2
    , printExprWith
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

import Control.Monad.State (evalState)
import Data.Patch
import Data.Tree (Tree)
import Data.Tree.View
import Data.Hash (Hashable)

import qualified Feldspar.Core.Reify as Syntactic
import Feldspar.Core.Reify hiding (desugar, sugar)
import qualified Feldspar.Core.Eval as E

import Feldspar.Compiler.Options (FeldOpts(..), FrontendPass(..), Options(..),
                                  PassCtrl(..), Pretty(..), Target(..),
                                  c99OpenMpPlatformOptions,
                                  c99PlatformOptions, defaultFeldOpts,
                                  defaultOptions, defaultPassCtrl, sicsOptions,
                                  sicsOptions2, sicsOptions3,
                                  tic64xPlatformOptions)
import Feldspar.Core.AdjustBindings (adjustBindings)
import Feldspar.Core.Middleend.CreateTasks
import Feldspar.Core.Middleend.Expand (expand)
import Feldspar.Core.Middleend.FromTyped (toU)
import Feldspar.Core.Middleend.LetSinking
import Feldspar.Core.Middleend.OptimizeUntyped
import Feldspar.Core.Middleend.PassManager (Prog(..), evalPasses, passC, passT)
import Feldspar.Core.Middleend.PushLets
import Feldspar.Core.Middleend.UniqueVars
import qualified Feldspar.Core.SizeProp as SP
import Feldspar.Core.Types
import Feldspar.Core.UntypedRepresentation (UntypedFeld, AUntypedFeld,
                                            annotate, prettyExp, rename,
                                            stringTree, stringTreeExp,
                                            unAnnotate)
import Feldspar.Core.Language
import Feldspar.Core.ValueInfo (ValueInfo, PrettyInfo(..))

-- The front-end driver.

instance Pretty UntypedFeld where
  pretty = pretty . annotate (const ())

instance PrettyInfo a => Pretty (AUntypedFeld a) where
  pretty = prettyExp f
     where f t x = " | " ++ prettyInfo t x

-- | Front-end driver
frontend :: Syntactic a
         => PassCtrl FrontendPass
         -> FeldOpts
         -> a
         -> ([String], Maybe UntypedFeld)
frontend ctrl opts prog = evalPasses 0
                   ( pc FPCreateTasks      (createTasks opts)
                   . pt FPUnAnnotate       unAnnotate
                   . pc FPUnique           uniqueVars
                   . pc FPExpand           expand
                   . pc FPPushLets         pushLets
                   . pc FPOptimize         optimize
                   . pc FPSinkLets         (sinkLets opts)
                   . pc FPRename           renameExp
                   . pt FPUntype           toU
                   . pc FPSizeProp         SP.sizeProp
                   . pc FPAdjustBind       adjustBindings
                   . pt FPUnASTF           unASTF
                   ) $ reifyFeld prog
  where pc :: Pretty a => FrontendPass -> (a -> a) -> Prog a Int -> Prog a Int
        pc = passC ctrl
        pt :: (Pretty a, Pretty b) => FrontendPass -> (a -> b) -> Prog a Int -> Prog b Int
        pt = passT ctrl

reifyFeld :: Syntactic a => a -> ASTF (Internal a)
reifyFeld = Syntactic.desugar

stringTreeASTF :: ASTF a -> Tree String
stringTreeASTF = stringTree . untypeUnOpt defaultFeldOpts

showDecorWith :: (ValueInfo -> String) -> ASTF a -> String
showDecorWith f = showTree . stringTreeExp g . untypeDecor defaultFeldOpts
  where g x = " in " ++ f x

showExpr :: Syntactic a => a -> String
showExpr = render . reifyFeld

-- | Show an untyped expression
showUntyped :: Syntactic a => FeldOpts -> a -> String
showUntyped opts = head . fst . frontend passCtrl opts
  where passCtrl = defaultPassCtrl{ wrBefore = [FPUnAnnotate]
                                  , stopBefore = [FPUnAnnotate]}

-- | Show an expression after a specific frontend pass
showUntyped' :: Syntactic a => FrontendPass -> FeldOpts -> a -> String
showUntyped' p opts = head . fst . frontend passCtrl opts
  where passCtrl = defaultPassCtrl{wrAfter = [p], stopAfter = [p]}

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
printExprWith _ = print . reifyFeld

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
drawDecor = putStrLn . showDecorWith show . reifyFeld

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
untype :: FeldOpts -> ASTF a -> UntypedFeld
untype opts = cleanUp opts
            . untypeDecor opts

-- | Untype and optimize.
untypeDecor :: FeldOpts -> ASTF a -> AUntypedFeld ValueInfo
untypeDecor opts = pushLets
                 . optimize
                 . sinkLets opts
                 . justUntype

-- | External module interface.
untypeUnOpt :: FeldOpts -> ASTF a -> UntypedFeld
untypeUnOpt opts = cleanUp opts
                 . justUntype

-- | Only do the conversion to AUntypedFeld ValueInfo
justUntype :: ASTF a -> AUntypedFeld ValueInfo
justUntype = renameExp . toU . SP.sizeProp . adjustBindings . unASTF

-- | Prepare the code for fromCore
cleanUp :: FeldOpts -> AUntypedFeld ValueInfo -> UntypedFeld
cleanUp opts = createTasks opts . unAnnotate . uniqueVars

renameExp :: AUntypedFeld a -> AUntypedFeld a
renameExp e = evalState (rename e) 0

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
