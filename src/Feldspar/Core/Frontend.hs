{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
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
    , Internal

    , FeldDomain
    , Data
    , SyntacticFeld
    , Syntax

    , module Frontend

    , FeldOpts
    , defaultFeldOpts
    , reifyFeld
    , reifyFeldM
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
    , evalTarget
    , desugar
    , sugar
    , resugar

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

import Control.Monad.State
import Test.QuickCheck

import Data.Patch
import Data.Tree.View
import Data.Hash

import Language.Syntactic hiding
    (desugar, sugar, resugar, showAST, drawAST, writeHtmlAST, stringTree)
import qualified Language.Syntactic as Syntactic
import qualified Language.Syntactic.Constructs.Decoration as Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder
import Language.Syntactic.Sharing.SimpleCodeMotion
import Language.Syntactic.Sharing.CodeMotion2
import Language.Syntactic.Sharing.SimpleCodeMotion3

import Feldspar.Range
import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Middleend.FromTyped
import Feldspar.Core.UntypedRepresentation (stringTree)
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Binding (cLambda)
import Feldspar.Core.Frontend.Array            as Frontend
import Feldspar.Core.Frontend.Binding          as Frontend
import Feldspar.Core.Frontend.Bits             as Frontend
import Feldspar.Core.Frontend.Complex          as Frontend
import Feldspar.Core.Frontend.Condition        as Frontend
import Feldspar.Core.Frontend.Conversion       as Frontend
import Feldspar.Core.Frontend.Elements         as Frontend
import Feldspar.Core.Frontend.Eq               as Frontend
import Feldspar.Core.Frontend.Error            as Frontend
import Feldspar.Core.Frontend.FFI              as Frontend
import Feldspar.Core.Frontend.Floating         as Frontend
import Feldspar.Core.Frontend.Fractional       as Frontend
import Feldspar.Core.Frontend.Future           as Frontend
import Feldspar.Core.Frontend.Integral         as Frontend
import Feldspar.Core.Frontend.Literal          as Frontend
import Feldspar.Core.Frontend.Logic            as Frontend
import Feldspar.Core.Frontend.Loop             as Frontend
import Feldspar.Core.Frontend.NoInline         as Frontend
import Feldspar.Core.Frontend.Num              as Frontend
import Feldspar.Core.Frontend.Ord              as Frontend
import Feldspar.Core.Frontend.Par              as Frontend
import Feldspar.Core.Frontend.Save             as Frontend
import Feldspar.Core.Frontend.SizeProp         as Frontend
import Feldspar.Core.Frontend.SourceInfo       as Frontend
import Feldspar.Core.Frontend.Switch           as Frontend
import Feldspar.Core.Frontend.RealFloat        as Frontend
import Feldspar.Core.Frontend.Tuple            as Frontend



prjDict :: PrjDict (Decor Info FeldDom)
prjDict = PrjDict
    (prjVariable prjDictFO . decorExpr)
    (prjLambda   prjDictFO . decorExpr)

mkId :: FeldOpts -> MkInjDict (Decor Info FeldDom)
mkId opts a b
  | simpleMatch (const . sharable) a
  , Just Dict <- typeDict a
  , Dict <- exprDictSub pTypeable b
  , Info {infoType = bType} <- getInfo b
  = case bType of
      FunType{} | P.not (SICS `inTarget` opts) -> Nothing
      _         -> Just InjDict
                            { injVariable = Decor (getInfo a) . injC . c' . Variable
                            , injLambda   = let info = ((mkInfoTy (FunType typeRep bType)) {infoSize = (infoSize (getInfo a), infoSize (getInfo b))})
                                            in Decor info . injC . cLambda
                            , injLet      = Decor (getInfo b) $ injC Let
                            }
mkId _ _ _ = Nothing


hoister opts
 | CSE `inTarget` opts
 = cm1 . optimize opts . stripDecor <=< cm2
 | otherwise = cm3
  where cm1 = codeMotion (simpleMatch (const . hoistOver)) prjDict (mkId opts)
        cm2 = codeMotion2 (simpleMatch (const . hoistOver)) prjDict (mkId opts)
        cm3 = codeMotion3 10 (simpleMatch (const . hoistOver)) prjDict (mkId opts) mkSubEnvDefault


reifyFeldM :: (SyntacticFeld a, MonadState VarId m)
    => FeldOpts
    -> BitWidth n
    -> a
    -> m (ASTF (Decor Info FeldDom) (Internal a))
reifyFeldM opts n =
    (   return
    .   optimize opts
    .   stripDecor
    <=< hoister opts
    .   optimize opts
    .   targetSpecialization n
    <=< reifyM
    .   fromFeld
    .   Syntactic.desugar
    )
  -- Note that it's important to do 'codeMotion' after 'optimize'. There may be
  -- sub-expressions that appear more than once in the original program, but
  -- where 'optimize' removes all but one occurrence. If 'codeMotion' was run
  -- first, these sub-expressions would be let bound, preventing subsequent
  -- optimizations.

-- | Reification and optimization of a Feldspar program
reifyFeld :: SyntacticFeld a
    => FeldOpts
    -> BitWidth n
    -> a
    -> ASTF (Decor Info FeldDom) (Internal a)
reifyFeld opts n = flip evalState 0 . reifyFeldM opts n

-- | Reification of a Feldspar program
reifyFeldUnOpt :: SyntacticFeld a
    => FeldOpts -> BitWidth n
    -> a
    -> ASTF FeldDom (Internal a)
reifyFeldUnOpt _ n = flip evalState 0 .
    (   return
    .   targetSpecialization n
    <=< reifyM
    .   fromFeld
    .   Syntactic.desugar
    )

showExpr :: SyntacticFeld a => a -> String
showExpr = render . reifyFeld defaultFeldOpts N32

-- | Print an optimized untyped expression
printExpr2 :: SyntacticFeld a => a -> IO ()
printExpr2 = printExpr2With defaultFeldOpts

-- | Draw the untyped syntax tree using unicode art
drawUntyped :: SyntacticFeld a => a -> IO ()
drawUntyped = drawUntypedWith defaultFeldOpts

-- | Draw the untyped syntax tree using unicode art
drawUntypedWith :: SyntacticFeld a => FeldOpts -> a -> IO ()
drawUntypedWith opts = drawTree . stringTree . untype opts . reifyFeld opts N32

-- | Print an optimized expression
printExpr :: SyntacticFeld a => a -> IO ()
printExpr = print . reifyFeld defaultFeldOpts N32

-- | Print an optimized untyped expression with options
printExpr2With :: SyntacticFeld a => FeldOpts -> a -> IO ()
printExpr2With opts = print . untype opts . reifyFeld opts N32

-- | Print an optimized expression with options
printExprWith :: SyntacticFeld a => FeldOpts -> a -> IO ()
printExprWith opts = print . reifyFeld opts N32

-- | Print an unoptimized expression
printExprUnOpt :: SyntacticFeld a => a -> IO ()
printExprUnOpt = print . reifyFeldUnOpt defaultFeldOpts N32

-- | Show the syntax tree using Unicode art
showAST :: SyntacticFeld a => a -> String
showAST = Syntactic.showAST . reifyFeld defaultFeldOpts N32

-- | Draw the syntax tree on the terminal using Unicode art
drawAST :: SyntacticFeld a => a -> IO ()
drawAST = Syntactic.drawAST . reifyFeld defaultFeldOpts N32

drawASTUnOpt :: SyntacticFeld a => a -> IO ()
drawASTUnOpt = Syntactic.drawAST . reifyFeldUnOpt defaultFeldOpts N32

-- | Write the syntax tree to an HTML file with foldable nodes
writeHtmlAST :: SyntacticFeld a => FilePath -> a -> IO ()
writeHtmlAST file = Syntactic.writeHtmlAST file . reifyFeld defaultFeldOpts N32

-- | Draw a syntax tree decorated with type and size information
showDecor :: SyntacticFeld a => a -> String
showDecor = Syntactic.showDecorWith show . reifyFeld defaultFeldOpts N32

-- | Draw a syntax tree decorated with type and size information
drawDecor :: SyntacticFeld a => a -> IO ()
drawDecor = Syntactic.drawDecorWith show . reifyFeld defaultFeldOpts N32

eval :: SyntacticFeld a => a -> Internal a
eval = evalBind . reifyFeld defaultFeldOpts N32

evalTarget
    :: ( SyntacticFeld a
       , BoundedInt (GenericInt U n)
       , BoundedInt (GenericInt S n)
       )
    => BitWidth n -> a -> Internal a
evalTarget n = evalBind . reifyFeld defaultFeldOpts n
  -- TODO This doesn't work yet, because 'targetSpecialization' is not implemented

desugar :: SyntacticFeld a => a -> Data (Internal a)
desugar = Syntactic.resugar

sugar :: SyntacticFeld a => Data (Internal a) -> a
sugar = Syntactic.resugar

resugar :: (SyntacticFeld a, SyntacticFeld b, Internal a ~ Internal b) => a -> b
resugar = Syntactic.resugar



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

