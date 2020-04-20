{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
       , Syntax(..)
       , ASTF(..)
       , unASTF
       , render
       , resugar
       , Data(..)
       , Mon(..)
       , CSEExpr(..)
       , (@@)
       , SugarF(..)
       , sugarSym
       , op2f
       , unFull
       , value
       , full
       ) where

import Feldspar.Core.Representation (Var(..), AExpr(..), Info(..), Expr(..),
                                     VarId, Op(..), fvi, CBind(..), TypeF(..),
                                     bvId, mkLets, sharable)
import qualified Feldspar.Core.Types as T
import Feldspar.Lattice (top)

import Control.Applicative
import Control.Monad.Cont
import Data.Complex (Complex(..))
import Data.Typeable
import Data.Hash (Hashable(..), asWord64, combine, hashInt, Hash)

import qualified Data.ByteString.Char8 as B
import Data.Array (Array, accumArray, (!), bounds, elems)
import Data.List (partition)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

resugar :: (Syntactic a, Syntactic b, Internal a ~ Internal b) => a -> b
resugar = sugar . desugar

-- | The CExpr component represents sharing in the expression using a map
--   which allows for linear time merges of two CExpr:s.
--   The Int component is used for generating unshadowed names in lambda
--   abstractions when reifying functions. It must be strictly greater than
--   the Int used for generating the bound variable of any lambda abstraction
--   in the expression part of the CExpr.
data ASTF a = ASTF (CExpr a) Int

-- | Convert an ASTF to an expression
unASTF :: b -> ASTF a -> AExpr a
unASTF _ (ASTF ce _) = snd $ catchBindings [] ce

render :: ASTF a -> String
render (ASTF (m,e) _) = unwords $ show e : "where" : map show (M.elems m)

instance Show (ASTF a) where
  show = render

class Syntactic a where
    type Internal a
    desugar :: a -> ASTF (Internal a)
    sugar   :: ASTF (Internal a) -> a

instance Syntactic (ASTF a)
  where
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

newtype Data a = Data { unData :: ASTF a }

deriving instance Typeable Data

instance Syntactic (Data a)
  where
    type Internal (Data a) = a
    desugar = unData
    sugar   = Data

instance Eq (Data a) where
    (==) _ _ = error "Eq (Data a): Binding time violation"

instance Show (Data a) where
    show = render . desugar . unData

-------------------------------------------------
-- Functions
-------------------------------------------------

instance (Syntax a, Syntactic b, TypeF (Internal b)) => Syntactic (a -> b) where
  type Internal (a -> b) = Internal a -> Internal b
  sugar e = error "sugar not implemented for a -> b"
  desugar f = ASTF (m1, Info top :& Lambda v e1) $ i + 1
    where ASTF ce i = desugar $ f (sugar $ ASTF (M.empty, Info top :& Variable v) 0)
          (m1, e1) = catchBindings [varNum v] ce
          v = Var (fromIntegral i + hashBase) B.empty

-- | User interface to embedded monadic programs
newtype Mon m a
  where
    Mon
        :: { unMon
              :: forall r . (Monad m, Typeable r, T.Type r, T.Type (m r))
              => Cont (ASTF (m r)) a
           }
        -> Mon m a

deriving instance Functor (Mon m)

instance Monad m => Monad (Mon m)
  where
    return a = Mon $ return a
    ma >>= f = Mon $ unMon ma >>= unMon . f

instance (Monad m, Applicative m) => Applicative (Mon m)
  where
    pure  = return
    (<*>) = ap

infixl 5 @@

-- | Construct an application
(@@) :: Syntactic a
     => (CSEExpr (Expr (Internal a -> b)), Int) -> a -> (CSEExpr (Expr b), Int)
(cf,i) @@ e = go $ desugar e
  where go (ASTF ce j) = (applyCSE cf ce, max i j)

-- | Convert an 'Op' to a function that builds the corresponding syntax tree
sugarSym :: SugarF a => Op (SugarT a) -> a
sugarSym = sugarF . op2f

-- | Mark an application as full rather than partial
newtype FFF a = FFF a

-- | Force the argument to be a full applicaton, resolving the
--   overloading in sugarSym
unFull :: FFF a -> a
unFull (FFF x) = x

op2f :: Op a -> (RCSExpr a, Int)
op2f op = ((M.empty, Operator op), 0)

type RCSExpr a = CSEExpr (Expr a)

-- | Support for the overloaded sugarSym function
class SugarF a where
  type SugarT a
  sugarF :: (RCSExpr (SugarT a), Int) -> a

instance (Syntactic b, SugarF c) => SugarF (b -> c) where
  type SugarT (b -> c) = Internal b -> SugarT c
  sugarF f = \ e -> sugarF $ f @@ e

instance (Syntactic b, TypeF (Internal b)) => SugarF (FFF b) where
  type SugarT (FFF b) = Internal b
  sugarF (~(m, e),i) = FFF $ sugar $ ASTF (flattenCSE (m, Info top :& e)) i

-------------------------------------------------
-- Converting Haskell values to Feldspar
-------------------------------------------------

value :: (Syntax a, Hashable (Internal a)) => Internal a -> a
value v = sugar $ full ((M.empty, Literal v), 0)

{- | Functions for incremental common subexpression elimination.
-}

type CSEMap = M.Map VarId CBind
type CSEExpr e = (CSEMap, e)
type CExpr a = CSEExpr (AExpr a)

-- | Convert a CSE map, an Expr and an Int to an ASTF
full :: T.Type b => (CSEExpr (Expr b), Int) -> ASTF b
full (~(m, e), i) = ASTF (flattenCSE (m, Info top :& e)) i

flattenCSE :: CExpr a -> CExpr a
flattenCSE (m,e) | not $ sharable e = (m, e)
flattenCSE (m,e@(i :& _))
  = mergeMapCExpr (M.singleton (varNum v) (CBind v e)) (m, i :& Variable v)
   where v = Var (hashExpr e) B.empty

applyCSE :: CSEExpr (Expr (a -> b)) -> CSEExpr (AExpr a) -> CSEExpr (Expr b)
applyCSE (lm, f) s@(_, (_ :& _)) = (m, f :@ e1)
   where (m, e1) = mergeMapCExpr lm s

mergeMapCExpr :: CSEMap -> CExpr a -> CExpr a
mergeMapCExpr lm (rm,e) = (M.union lm rm1, e1)
   where sect = M.intersectionWith (,) lm rm
         (rm1,e1) = if null colls then (rm,e) else catchBindings (map fst colls) (rm,e)
         colls = filter (uncurry (/=) . snd) $ M.toList sect

hashError :: M.Map VarId (CBind,CBind) -> a
hashError sect = error $ "CSE.mergeCSE: hash conflict, diff is" ++ concatMap showDiff diffs
  where diffs = filter (uncurry (/=)) $ M.elems sect
        showDiff (b1,b2) = "\nDiff for " ++ show (bvId b1) ++ "\n"
--                                    ++ showRhs b1 ++ "\n--\n" ++ showRhs b2

{- | Functions for floating bindings out of lambdas whenever possible.
-}

floatBindings :: [VarId] -> CSEMap -> (CSEMap, [CBind])
floatBindings vs bm = (M.fromAscList [(bvId b, b) | b <- fbs], concat nfBss)
   where arr = accumArray f [] (0, fromIntegral $ len-1)
             $ map toPair
             $ [(v, bindThreshold) | v <- vs] ++ map (depthBind arr) bs
         arr :: Array VarId [(VarId,Int)]
         toPair vx@(v,x) = (v `mod` len, vx)
         m = fromIntegral $ n + n `div` 8
         len = head $ filter (>= m) $ iterate (*2) 2 :: VarId
         f xs x = x:xs
         n = M.size bm
         bs = M.elems bm
         (fbs, nfbs)
           | null vs   = ([], bs)
           | otherwise = partition ((< bindThreshold) . hashLook arr . bvId) bs
         nfArr = accumArray
                    (flip (:))
                    []
                    (0,n)
                    [(hashLook arr (fromIntegral $ bvId b) `mod` bindThreshold, b) | b <- nfbs]
         nfBss = [reverse bs | bs <- elems nfArr, not $ null bs]

hashLook :: Array VarId [(VarId,Int)] -> VarId -> Int
hashLook m i = maximum $ 0 : [d | (v,d) <- m ! (i `mod` (snd (bounds m) + 1)), v == i]

depthBind :: Array VarId [(VarId,Int)] -> CBind -> (VarId, Int)
depthBind arr (CBind v e) = (varNum v, maximum (0 : map (hashLook arr) (S.toList $ fvi e)) + 1)

bindThreshold = 1000000

catchBindings :: [VarId] -> CExpr a -> CExpr a
catchBindings vs (m,e) = (m1, mkLets (bs,e))
  where (m1,bs) = floatBindings vs m

{- | Functions for constructing hash values.
-}

hashExpr :: AExpr a -> VarId
hashExpr (_ :& (e :: Expr a)) = (hash2VarId $ hash (typeRepF :: T.TypeRep a)) `combineHash` hashExprR e

hashExprR :: Expr a -> VarId
hashExprR (Variable v) = varNum v
hashExprR (Literal c) = hash2VarId $ hash c
hashExprR (Operator op) = hashOp op
-- Hash value of rhs in let is equal to bound variable name which occurs in body
hashExprR (Operator Let :@ _ :@ (_ :& Lambda _ e)) = hashExpr e
hashExprR (f :@ e) = appHash `combineHash` hashExprR f `combineHash` hashExpr e
hashExprR (Lambda v e) = absHash `combineHash` varNum v `combineHash` hashExpr e

hashStr :: String -> VarId
hashStr s = fromInteger $ foldr combineHash 5 $ map (toInteger . fromEnum) s

hashOp :: Op a -> VarId
hashOp op = hashStr $ show op

hash2VarId :: Hash -> VarId
hash2VarId h = fromIntegral $ (fromIntegral $ asWord64 h) `mod` hashMod

--------------------------------------------------------------------------------
-- * Hashing
--------------------------------------------------------------------------------

instance Hashable a => Hashable (Complex a) where
  hash (re :+ im) = hash re `combine` hash im

instance Hashable (T.Signedness a) where
  hash T.U = hashInt 1
  hash T.S = hashInt 2

instance Hashable (T.BitWidth a) where
  hash T.N8      = hashInt 1
  hash T.N16     = hashInt 2
  hash T.N32     = hashInt 3
  hash T.N64     = hashInt 4
  hash T.NNative = hashInt 5

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
  hash (T.TargetArrType sz t)       = hashInt 8 # sz # t
  hash (T.Tup2Type a b)             = hashInt 9 # a # b
  hash (T.Tup3Type a b c)           = hashInt 10 # a # b # c
  hash (T.Tup4Type a b c d)         = hashInt 11 # a # b # c # d
  hash (T.Tup5Type a b c d e)       = hashInt 12 # a # b # c # d # e
  hash (T.Tup6Type a b c d e f)     = hashInt 13 # a # b # c # d # e # f
  hash (T.Tup7Type a b c d e f g)   = hashInt 14 # a # b # c # d # e # f # g
  hash (T.Tup8Type a b c d e f g h) = hashInt 15 # a # b # c # d # e # f # g # h
  hash (T.Tup9Type a b c d e f g h i)
       = hashInt 16 # a # b # c # d # e # f # g # h # i
  hash (T.Tup10Type a b c d e f g h i j)
       = hashInt 17 # a # b # c # d # e # f # g # h # i # j
  hash (T.Tup11Type a b c d e f g h i j k)
       = hashInt 18 # a # b # c # d # e # f # g # h # i # j # k
  hash (T.Tup12Type a b c d e f g h i j k l)
       = hashInt 19 # a # b # c # d # e # f # g # h # i # j # k # l
  hash (T.Tup13Type a b c d e f g h i j k l m)
       = hashInt 20 # a # b # c # d # e # f # g # h # i # j # k # l # m
  hash (T.Tup14Type a b c d e f g h i j k l m n)
       = hashInt 21 # a # b # c # d # e # f # g # h # i # j # k # l # m # n
  hash (T.Tup15Type a b c d e f g h i j k l m n o)
       = hashInt 22 # a # b # c # d # e # f # g # h # i # j # k # l # m # n # o
  hash (T.FunType a b)    = hashInt 23 # a # b
  hash (T.MutType t)      = hashInt 24 # t
  hash (T.RefType t)      = hashInt 25 # t
  hash (T.MArrType t)     = hashInt 26 # t
  hash (T.ParType t)      = hashInt 27 # t
  hash (T.ElementsType t) = hashInt 28 # t
  hash (T.ConsType a b)   = hashInt 31 # a # b
  hash  T.NilType         = hashInt 32
  hash (T.TupleType t)    = hashInt 33 # t
  hash (T.IVarType t)     = hashInt 29 # t
  hash (T.FValType t)     = hashInt 30 # t

appHash, absHash :: VarId
appHash = 655360 + 40960 + 2560 + 160 + 10 + 17
absHash = 327680 + 20480 + 1280 +  80 +  5

combineHashL :: Integral a => [a] -> a
combineHashL = foldr combineHash 0

combineHash :: Integral a => a -> a -> a
combineHash l r = fromInteger $ mod (toInteger l + 127 * toInteger r) hashMod

hashMod :: Integer
hashMod = 1024 * 1024 * 1024 * 1024 * 1023 + 1

hashBase :: VarId
hashBase = 10000 * 1000000 * 1000000
