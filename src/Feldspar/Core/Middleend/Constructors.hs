{-# OPTIONS_GHC -Wall #-}

--
-- Copyright (c) 2019, ERICSSON AB
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

module Feldspar.Core.Middleend.Constructors
  ( Bag(..)
  , BindBag
  , AExpB
  , RExpB
  , variable
  , literal
  , lambda
  , app
  , aIn
  , mkBinds
  , toExpr
  , fromExpr
  , fromRExpr
  , unAnnotateB
  )
  where

import Feldspar.Core.UntypedRepresentation

-- | Intelligent constructors for placing bindings

data Bag a = Bags [Bag a]
           | Item a
           deriving (Eq, Ord, Show)

type BindBag a = Bag [(Var, UntypedFeld a)]

foldBag :: (a -> b -> b) -> b -> Bag a -> b
foldBag f u (Bags bs) = foldr (\ b r -> foldBag f r b) u bs
foldBag f u (Item x) = f x u

appendBag :: Bag a -> Bag a -> Bag a
appendBag (Bags []) b         = b
appendBag b         (Bags []) = b
appendBag l         (Bags rs) = Bags $ l : rs
appendBag l         (Item r)  = Bags [l, Item r]

concatBags :: [Bag a] -> Bag a
concatBags = foldr appendBag (Bags [])

type AExpB a = (BindBag a, UntypedFeld a)
type RExpB a = (BindBag a, RRExp a)

type RRExp a = UntypedFeldF (UntypedFeld a)

toExpr :: AExpB a -> UntypedFeld a
toExpr (b,e) = foldBag (curry mkLets) e b

fromExpr :: UntypedFeld a -> AExpB a
fromExpr e = (Bags [], e)

unAnnotateB :: AExpB a -> RExpB a
unAnnotateB (b, In _ e) = (b, e)

fromRExpr :: RRExp a -> RExpB a
fromRExpr e = (Bags [], e)

variable :: Var -> RExpB a
variable v = (Bags [], Variable v)

literal :: Lit -> RExpB a
literal l = (Bags [], Literal l)

lambda :: Var -> AExpB a -> RExpB a
lambda v eb = (Bags [], Lambda v $ toExpr eb)

app :: Op -> Type -> [AExpB a] -> RExpB a
app Condition t [(b,ec), et, ee] = (b, App Condition t [ec, toExpr et, toExpr ee])
app p t [be] | p `elem` [MkFuture, ParFork] = (Bags [], App p t [toExpr be])
app op t es = (concatBags bs, App op t es1)
  where (bs,es1) = unzip es

aIn :: a -> RExpB a -> AExpB a
aIn r (b,e) = (b, In r e)

mkBinds :: ([(Var, AExpB a)], AExpB a) -> AExpB a
mkBinds (bs,(b,e)) = (foldr appendBag (appendBag (Item $ zip vs es1) b) bs1, e)
  where (vs,bes) = unzip bs
        (bs1,es1) = unzip bes
