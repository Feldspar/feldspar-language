{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}
-- Hashable instances give orphan warnings.
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Feldspar.Core.Reify
       ( Syntactic(..)
       , Syntax
       , ASTF
       , unASTF
       , render
       , resugar
       , Data(..)
       , Mon(..)
       , SugarF
       , sugarSym
       , unFull
       , value
       ) where

import Feldspar.Compiler.Options (Pretty(..))
import Feldspar.Core.Representation (Var(..), AExpr(..), Info(..), Expr(..),
                                     VarId, Op(..), fvi, CBind(..), TypeF(..),
                                     bvId, mkLets, sharable)
import qualified Feldspar.Core.Types as T
import Feldspar.Lattice (top)
import Feldspar.Core.NestedTuples

import Control.Monad.Cont (Cont, ap)
import Data.Complex (Complex(..))
import Data.Typeable (Typeable, typeOf)
import Data.Hash (Hashable(..), asWord64, combine, hashInt, Hash)

import qualified Data.ByteString.Char8 as B
import Data.Array (Array, accumArray, (!), bounds, elems)
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Lazy.Search as LB
import Data.Char (chr)
import Data.List (partition)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import GHC.TypeLits

{-

Debugging test output differences
---------------------------------

The variables in the test outputs can sometimes vary between
GHC versions. Feldspar should be deterministic so this is usually
caused by differences in the Show instance for TypeRep.

Debug the difference by inserting #define DEBUG_NORMALIZATION,
starting ghci on the affected test code and run "icompile expr".
Before the code is output there are a number of lines on the form:

Normalized: Op Int32

which shows the normalized output. Start a ghci from the version
of GHC that does not pass the tests and do the same. Look for differences
and add a substitution for the differences you find to normalizeTypeRep.
The usual culprit is some infix type constructor that gets additional
parentheses, since we only need a unique thing to hash we can just
strip the parentheses.

-}

#ifdef DEBUG_NORMALIZATION
import Debug.Trace
#endif

resugar :: (Syntactic a, Syntactic b, Internal a ~ Internal b) => a -> b
resugar = sugar . desugar

-- | The CExpr component represents sharing in the expression using a map
--   which allows for linear time merges of two CExpr:s.
--   The Int component is used for generating unshadowed names in lambda
--   abstractions when reifying functions. It must be strictly greater than
--   the Int used for generating the bound variable of any lambda abstraction
--   in the expression part of the CExpr.
data ASTF a = ASTF (CExpr a) Int

instance Pretty (ASTF a) where
  pretty = render

-- | Convert an ASTF to an expression
unASTF :: ASTF a -> AExpr a
unASTF (ASTF ce _) = snd $ catchBindings [] ce

render :: ASTF a -> String
render (ASTF (m,e) _) = unwords $ show e : "where" : map show (M.elems m)

instance Show (ASTF a) where
  show = render

class Syntactic a where
    type Internal a
    desugar :: a -> ASTF (Internal a)
    sugar   :: ASTF (Internal a) -> a

instance Syntactic (ASTF a) where
    {-# SPECIALIZE instance Syntactic (ASTF a) #-}
    type Internal (ASTF a) = a
    desugar = id
    sugar   = id
    {-# INLINABLE desugar #-}
    {-# INLINABLE sugar #-}

-- | Specialization of the 'Syntactic' class for first class values (eg not functions)
class    (Syntactic a, T.Type (Internal a)) => Syntax a
instance (Syntactic a, T.Type (Internal a)) => Syntax a
  -- It would be possible to let 'Syntax' be an alias instead of giving separate
  -- instances for all types. However, this leads to horrible error messages.
  -- For example, if 'Syntax' is an alias, the following expression gives a huge
  -- type error:
  --
  -- > eval (forLoop 10 0 (const (+id)))
  --
  -- The type error is not very readable now either, but at least it fits on the
  -- screen.

--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

instance Syntactic () where
    type Internal () = ()
    desugar = value
    sugar _ = ()

newtype Data a = Data { unData :: ASTF a }

instance Syntactic (Data a) where
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

instance Eq (Data a) where
    (==) _ _ = error "Eq (Data a): Binding time violation"

instance Show (Data a) where
  show = render . desugar

-------------------------------------------------
-- Functions
-------------------------------------------------

instance (Syntax a, Syntactic b, TypeF (Internal b)) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a -> Internal b
  sugar _ = error "sugar not implemented for a -> b"
  desugar f = ASTF (m1, Info top :& Sym (Lambda v) :@ e1) $ i + 1
    where ASTF ce i = desugar $ f (sugar $ ASTF (M.empty, Info top :& Sym (Variable v)) 0)
          (m1, e1) = catchBindings [varNum v] ce
          v = Var (fromIntegral i + hashBase) B.empty

-- | User interface to embedded monadic programs
newtype Mon m a where
    Mon
        :: { unMon
              :: forall r . (Monad m, Typeable r, T.Type r, T.Type (m r))
              => Cont (ASTF (m r)) a
           }
        -> Mon m a

deriving instance Functor (Mon m)

instance Monad m => Monad (Mon m) where
    return a = Mon $ return a
    ma >>= f = Mon $ unMon ma >>= unMon . f

instance (Monad m, Applicative m) => Applicative (Mon m) where
    pure  = return
    (<*>) = ap

-- | Convert an 'Op' to a function that builds the corresponding syntax tree
sugarSym :: (Typeable (SugarT a), SugarF a) => Op (SugarT a) -> a
sugarSym op = sugarF ((M.empty, Sym op), 0)

-- | Mark an application as full rather than partial
newtype FFF a = FFF a

-- | Force the argument to be a full applicaton, resolving the
--   overloading in sugarSym
unFull :: FFF a -> a
unFull (FFF x) = x

-- | Support for the overloaded sugarSym function
class SugarF a where
  type SugarT a
  sugarF :: (CSEExpr (Expr (SugarT a)), Int) -> a

instance (Syntactic b, SugarF c) => SugarF (b -> c) where
  type SugarT (b -> c) = Internal b -> SugarT c
  sugarF (cf, i) = sugarF . go . desugar
    where go (ASTF ce j) = (applyCSE cf ce, max i j)

instance Syntax b => SugarF (FFF b) where
  type SugarT (FFF b) = Internal b
  sugarF = FFF . sugar . full

-------------------------------------------------
-- Converting Haskell values to Feldspar
-------------------------------------------------

value :: (Syntax a, Hashable (Internal a)) => Internal a -> a
value v = unFull $ sugarSym (Literal v)

{- | Functions for incremental common subexpression elimination.
-}

type CSEMap = M.Map VarId CBind
type CSEExpr e = (CSEMap, e)
type CExpr a = CSEExpr (AExpr a)

-- | Convert a CSE map, an Expr and an Int to an ASTF
full :: T.Type b => (CSEExpr (Expr b), Int) -> ASTF b
full (~(m, e), i) = ASTF (flattenCSE (m, Info top :& e)) i

-- TODO: Document in what sense this flattens CSE.
flattenCSE :: T.Type a => CExpr a -> CExpr a
flattenCSE (m,e) | not $ sharable e = (m, e)
flattenCSE (m,e@(i :& _))
  = mergeMapCExpr (M.singleton (varNum v) (CBind v e)) (m, i :& Sym (Variable v))
   where v = Var (hashExpr e) B.empty

-- TODO: Document in what sense this applies CSE.
applyCSE :: CSEExpr (Expr (a -> b)) -> CSEExpr (AExpr a) -> CSEExpr (Expr b)
applyCSE (lm, f) s@(_, _ :& _) = (m, f :@ e1)
   where (m, e1) = mergeMapCExpr lm s

mergeMapCExpr :: CSEMap -> CExpr a -> CExpr a
mergeMapCExpr lm e@(rm, _) = (M.union lm rm1, e1)
   where (rm1, e1) | null collisions = e
                   | otherwise = catchBindings (M.keys collisions) e
         collisions = M.filter (uncurry (/=)) $ M.intersectionWith (,) lm rm

{- | Functions for floating bindings out of lambdas whenever possible.
-}

-- TODO: Document this function, the logic is not trivial.
floatBindings :: [VarId] -> CSEMap -> (CSEMap, [CBind])
floatBindings vs bm = (M.fromAscList [(bvId b, b) | b <- fbs], concat nfBss)
  where arr :: Array VarId [(VarId, Int)]
        arr = accumArray (flip (:)) [] (0, fromIntegral $ len - 1)
            $ map toPair
            $ [(v, bindThreshold) | v <- vs] ++ map (depthBind arr) bs'
        -- TODO Rename toPair, it is some kind of cap+nest.
        toPair vx@(v, _) = (v `mod` len, vx)
        m = fromIntegral $ n + n `div` 8
        len = head $ filter (>= m) $ iterate (*2) 2 :: VarId
        n = M.size bm
        bs' = M.elems bm
        (fbs, nfbs)
          | null vs   = ([], bs')
          | otherwise = partition ((< bindThreshold) . hashLook arr . bvId) bs'
        nfArr = accumArray (flip (:)) [] (0, n)
                 [(hashLook arr (fromIntegral $ bvId b) `mod` bindThreshold, b)
                 | b <- nfbs]
        nfBss = [reverse bs | bs <- elems nfArr, not $ null bs]

-- TODO: Document (is this some kind of lookup on arrays?).
hashLook :: Array VarId [(VarId, Int)] -> VarId -> Int
hashLook m i
  = maximum $ 0:[d | (v, d) <- m ! (i `mod` (snd (bounds m) + 1)), v == i]

-- TODO: Document what this computes.
depthBind :: Array VarId [(VarId, Int)] -> CBind -> (VarId, Int)
depthBind arr (CBind v e) -- TODO: is the second component S.findMax?
  = (varNum v, maximum (0:map (hashLook arr) (S.toList $ fvi e)) + 1)

bindThreshold :: Int
bindThreshold = 1000000

-- TODO: In what sense does this catch bindings?
catchBindings :: [VarId] -> CExpr a -> CExpr a
catchBindings vs (m, e) = (m1, mkLets (bs,e))
  where (m1, bs) = floatBindings vs m

{- | Functions for constructing hash values.
-}

hashExpr :: AExpr a -> VarId
hashExpr = fromIntegral . asWord64 . hashExpr'

hashExpr' :: AExpr a -> Hash
hashExpr' (_ :& e) = hashExprR e

hashExprR :: Expr a -> Hash
-- Hash the type into the hash value to avoid hashing different
-- F2I/I2F type instantiations to the same value.
hashExprR (Sym op)
  = (hash . normalizeTypeRep . show $ typeOf op) `combine` hash (show op)
-- Hash value of rhs in let is equal to bound variable name which occurs in body
hashExprR (Sym Let :@ _ :@ (_ :& Sym (Lambda _) :@ e)) = hashExpr' e
hashExprR (f :@ e) = hashInt appHash `combine` hashExprR f `combine` hashExpr' e

-- | Normalizes show output for TypeRep across GHC versions.
--
--   The output of show of a TypeRep varies across GHC versions, so
--   patch up the output in the simplest way possible to get stable
--   variable names in our tests. One GHC bug relating to this is #15236.
normalizeTypeRep :: String -> String
#ifdef DEBUG_NORMALIZATION
normalizeTypeRep s = trace ("Normalized: " ++ out ++ "\n") out
#else
normalizeTypeRep s = out -- Note [Debugging test output differences].
#endif
  where out = map (chr . fromEnum) (LB.unpack out')
        out' = LB.replace (B.pack "(:*)") (B.pack ":*") s02
        s01 = LB.replace (B.pack "(->)") (B.pack "->") (LB.pack s)
        s02 = LB.replace (B.pack "('[] *)") (B.pack "'[]") s01

--------------------------------------------------------------------------------
-- * Hashing
--------------------------------------------------------------------------------

instance Hashable a => Hashable (Complex a) where
  hash (re :+ im) = hash re `combine` hash im

instance KnownSymbol a => Hashable (T.Signedness a) where
  hash = hash . symbolVal

instance KnownNat a => Hashable (T.BitWidth a) where
  hash = hashInt . fromIntegral . natVal

-- This instance is needed to allow nested tuples as literals (by the value function)
instance HashTup a => Hashable (Tuple a) where
  hash = hashTup

class HashTup a where
  hashTup :: Tuple a -> Hash

instance HashTup '[] where
  hashTup _ = hashInt 1

instance (Hashable h, HashTup t) => HashTup (h ': t) where
  hashTup (x :* xs) = hashInt 2 # x # xs

infixl 5 #
(#) :: Hashable a => Hash -> a -> Hash
h # x = h `combine` hash x

instance Hashable (T.TypeRep a) where
  hash T.UnitType                   = hashInt 1
  hash T.BoolType                   = hashInt 2
  hash (T.IntType sgn sz)           = hashInt 3 # sgn # sz
  hash T.FloatType                  = hashInt 4
  hash T.DoubleType                 = hashInt 5
  hash (T.ComplexType t)            = hashInt 6 # t
  hash (T.ArrayType t)              = hashInt 7 # t
  hash (T.Tup2Type t)               = hashInt 8 # t
  hash (T.Tup3Type t)               = hashInt 9 # t
  hash (T.Tup4Type t)               = hashInt 10 # t
  hash (T.Tup5Type t)               = hashInt 11 # t
  hash (T.Tup6Type t)               = hashInt 12 # t
  hash (T.Tup7Type t)               = hashInt 13 # t
  hash (T.Tup8Type t)               = hashInt 14 # t
  hash (T.Tup9Type t)               = hashInt 15 # t
  hash (T.Tup10Type t)              = hashInt 16 # t
  hash (T.Tup11Type t)              = hashInt 17 # t
  hash (T.Tup12Type t)              = hashInt 18 # t
  hash (T.Tup13Type t)              = hashInt 19 # t
  hash (T.Tup14Type t)              = hashInt 20 # t
  hash (T.Tup15Type t)              = hashInt 21 # t
  -- Index 22 available.
  hash (T.FunType a b)    = hashInt 23 # a # b
  hash (T.MutType t)      = hashInt 24 # t
  hash (T.RefType t)      = hashInt 25 # t
  hash (T.MArrType t)     = hashInt 26 # t
  hash (T.ParType t)      = hashInt 27 # t
  hash (T.ElementsType t) = hashInt 28 # t
  hash (T.ConsType a b)   = hashInt 31 # a # b
  hash  T.NilType         = hashInt 32
  -- Index 33 avalable.
  hash (T.IVarType t)     = hashInt 29 # t
  hash (T.FValType t)     = hashInt 30 # t

appHash :: Int
appHash = 655360 + 40960 + 2560 + 160 + 10 + 17

hashBase :: VarId
hashBase = 10000 * 1000000 * 1000000
