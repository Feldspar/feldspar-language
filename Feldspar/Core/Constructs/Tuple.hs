{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.Constructs.Tuple
    ( module Language.Syntactic.Constructs.Tuple
    ) where

import Data.Tuple.Select

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Tuple

import Feldspar.Core.Types
import Feldspar.Core.Interpretation

instance Sharable Tuple

{-
instance SizeProp (Tuple TypeCtx)
  where
    sizeProp Tup2 (a :* b :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        = (infoSize ia, infoSize ib)
    sizeProp Tup3 (a :* b :* c :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          )
    sizeProp Tup4 (a :* b :* c :* d :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          )
    sizeProp Tup5 (a :* b :* c :* d :* e :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          )
    sizeProp Tup6 (a :* b :* c :* d :* e :* g :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          )
    sizeProp Tup7 (a :* b :* c :* d :* e :* g :* h :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          )
-}

instance Sharable Select
  where
    sharable _ = False

sel1Size :: (Sel1' a ~ b) => TypeRep a -> Size a -> Size b
sel1Size Tup2Type{} = sel1
sel1Size Tup3Type{} = sel1
sel1Size Tup4Type{} = sel1
sel1Size Tup5Type{} = sel1
sel1Size Tup6Type{} = sel1
sel1Size Tup7Type{} = sel1

sel2Size :: (Sel2' a ~ b) => TypeRep a -> (Size a -> Size b)
sel2Size Tup2Type{} = sel2
sel2Size Tup3Type{} = sel2
sel2Size Tup4Type{} = sel2
sel2Size Tup5Type{} = sel2
sel2Size Tup6Type{} = sel2
sel2Size Tup7Type{} = sel2

sel3Size :: (Sel3' a ~ b) => TypeRep a -> (Size a -> Size b)
sel3Size Tup3Type{} = sel3
sel3Size Tup4Type{} = sel3
sel3Size Tup5Type{} = sel3
sel3Size Tup6Type{} = sel3
sel3Size Tup7Type{} = sel3

sel4Size :: (Sel4' a ~ b) => TypeRep a -> (Size a -> Size b)
sel4Size Tup4Type{} = sel4
sel4Size Tup5Type{} = sel4
sel4Size Tup6Type{} = sel4
sel4Size Tup7Type{} = sel4

sel5Size :: (Sel5' a ~ b) => TypeRep a -> (Size a -> Size b)
sel5Size Tup5Type{} = sel5
sel5Size Tup6Type{} = sel5
sel5Size Tup7Type{} = sel5

sel6Size :: (Sel6' a ~ b) => TypeRep a -> (Size a -> Size b)
sel6Size Tup6Type{} = sel6
sel6Size Tup7Type{} = sel6

sel7Size :: (Sel7' a ~ b) => TypeRep a -> (Size a -> Size b)
sel7Size Tup7Type{} = sel7

{-
instance SizeProp (Select TypeCtx)
  where
    sizeProp Sel1 (WrapFull ia :* Nil) =
        sel1Size (infoType ia) (infoSize ia)
    sizeProp Sel2 (WrapFull ia :* Nil) =
        sel2Size (infoType ia) (infoSize ia)
    sizeProp Sel3 (WrapFull ia :* Nil) =
        sel3Size (infoType ia) (infoSize ia)
    sizeProp Sel4 (WrapFull ia :* Nil) =
        sel4Size (infoType ia) (infoSize ia)
    sizeProp Sel5 (WrapFull ia :* Nil) =
        sel5Size (infoType ia) (infoSize ia)
    sizeProp Sel6 (WrapFull ia :* Nil) =
        sel6Size (infoType ia) (infoSize ia)
    sizeProp Sel7 (WrapFull ia :* Nil) =
        sel7Size (infoType ia) (infoSize ia)
-}

-- | Compute a witness that a symbol and an expression have the same result type
tupEq :: Type (DenResult a) =>
    sym a -> ASTF (Decor Info dom) b -> Maybe (TypeEq (DenResult a) b)
tupEq _ b = typeEq typeRep (infoType $ getInfo b)

{-
instance
    ( Tuple TypeCtx :<: dom
    , Select TypeCtx :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize (Tuple TypeCtx) dom
  where
    constructFeatOpt tup@Tup2 (s1 :* s2 :* Nil)
        | (prjDecorCtx typeCtx -> Just (_,Sel1)) :$ a <- s1
        , (prjDecorCtx typeCtx -> Just (_,Sel2)) :$ b <- s2
        , alphaEq a b
        , TypeWit     <- fromSatWit $ witnessSat tup
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt tup@Tup3 (s1 :* s2 :* s3 :* Nil)
        | (prjDecorCtx typeCtx -> Just (_,Sel1)) :$ a <- s1
        , (prjDecorCtx typeCtx -> Just (_,Sel2)) :$ b <- s2
        , (prjDecorCtx typeCtx -> Just (_,Sel3)) :$ c <- s3
        , alphaEq a b
        , alphaEq a c
        , TypeWit     <- fromSatWit $ witnessSat tup
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt tup@Tup4 (s1 :* s2 :* s3 :* s4 :* Nil)
        | (prjDecorCtx typeCtx -> Just (_,Sel1)) :$ a <- s1
        , (prjDecorCtx typeCtx -> Just (_,Sel2)) :$ b <- s2
        , (prjDecorCtx typeCtx -> Just (_,Sel3)) :$ c <- s3
        , (prjDecorCtx typeCtx -> Just (_,Sel4)) :$ d <- s4
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , TypeWit     <- fromSatWit $ witnessSat tup
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt tup@Tup5 (s1 :* s2 :* s3 :* s4 :* s5 :* Nil)
        | (prjDecorCtx typeCtx -> Just (_,Sel1)) :$ a <- s1
        , (prjDecorCtx typeCtx -> Just (_,Sel2)) :$ b <- s2
        , (prjDecorCtx typeCtx -> Just (_,Sel3)) :$ c <- s3
        , (prjDecorCtx typeCtx -> Just (_,Sel4)) :$ d <- s4
        , (prjDecorCtx typeCtx -> Just (_,Sel5)) :$ e <- s5
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , TypeWit     <- fromSatWit $ witnessSat tup
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt tup@Tup6 (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* Nil)
        | (prjDecorCtx typeCtx -> Just (_,Sel1)) :$ a <- s1
        , (prjDecorCtx typeCtx -> Just (_,Sel2)) :$ b <- s2
        , (prjDecorCtx typeCtx -> Just (_,Sel3)) :$ c <- s3
        , (prjDecorCtx typeCtx -> Just (_,Sel4)) :$ d <- s4
        , (prjDecorCtx typeCtx -> Just (_,Sel5)) :$ e <- s5
        , (prjDecorCtx typeCtx -> Just (_,Sel6)) :$ f <- s6
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , TypeWit     <- fromSatWit $ witnessSat tup
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt tup@Tup7 (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* Nil)
        | (prjDecorCtx typeCtx -> Just (_,Sel1)) :$ a <- s1
        , (prjDecorCtx typeCtx -> Just (_,Sel2)) :$ b <- s2
        , (prjDecorCtx typeCtx -> Just (_,Sel3)) :$ c <- s3
        , (prjDecorCtx typeCtx -> Just (_,Sel4)) :$ d <- s4
        , (prjDecorCtx typeCtx -> Just (_,Sel5)) :$ e <- s5
        , (prjDecorCtx typeCtx -> Just (_,Sel6)) :$ f <- s6
        , (prjDecorCtx typeCtx -> Just (_,Sel7)) :$ g <- s7
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , TypeWit     <- fromSatWit $ witnessSat tup
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt feat args = constructFeatUnOpt feat args

    constructFeatUnOpt = constructFeatUnOptDefault


instance
    ( Select TypeCtx :<: dom
    , Tuple TypeCtx :<: dom
    , Optimize dom dom
    ) =>
      Optimize (Select TypeCtx) dom
  where
    constructFeatOpt Sel1 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup2)) :$ a :$ _) <- t                          = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup3)) :$ a :$ _ :$ _) <- t                     = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup4)) :$ a :$ _ :$ _ :$ _) <- t                = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup5)) :$ a :$ _ :$ _ :$ _ :$ _) <- t           = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup6)) :$ a :$ _ :$ _ :$ _ :$ _ :$ _) <- t      = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ a :$ _ :$ _ :$ _ :$ _ :$ _ :$ _) <- t = return a

    constructFeatOpt Sel2 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup2)) :$ _ :$ a) <- t                          = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup3)) :$ _ :$ a :$ _) <- t                     = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup4)) :$ _ :$ a :$ _ :$ _) <- t                = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup5)) :$ _ :$ a :$ _ :$ _ :$ _) <- t           = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup6)) :$ _ :$ a :$ _ :$ _ :$ _ :$ _) <- t      = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ _ :$ a :$ _ :$ _ :$ _ :$ _ :$ _) <- t = return a

    constructFeatOpt Sel3 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup3)) :$ _ :$ _ :$ a) <- t                     = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup4)) :$ _ :$ _ :$ a :$ _) <- t                = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup5)) :$ _ :$ _ :$ a :$ _ :$ _) <- t           = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup6)) :$ _ :$ _ :$ a :$ _ :$ _ :$ _) <- t      = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ _ :$ _ :$ a :$ _ :$ _ :$ _ :$ _) <- t = return a

    constructFeatOpt Sel4 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup4)) :$ _ :$ _ :$ _ :$ a) <- t                = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup5)) :$ _ :$ _ :$ _ :$ a :$ _) <- t           = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup6)) :$ _ :$ _ :$ _ :$ a :$ _ :$ _) <- t      = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ _ :$ _ :$ _ :$ a :$ _ :$ _ :$ _) <- t = return a

    constructFeatOpt Sel5 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup5)) :$ _ :$ _ :$ _ :$ _ :$ a) <- t           = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup6)) :$ _ :$ _ :$ _ :$ _ :$ a :$ _) <- t      = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ _ :$ _ :$ _ :$ _ :$ a :$ _ :$ _) <- t = return a

    constructFeatOpt Sel6 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup6)) :$ _ :$ _ :$ _ :$ _ :$ _ :$ a) <- t      = return a
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ _ :$ _ :$ _ :$ _ :$ _ :$ a :$ _) <- t = return a

    constructFeatOpt Sel7 (t :* Nil)
        | ((prjDecorCtx typeCtx -> Just (_,Tup7)) :$ _ :$ _ :$ _ :$ _ :$ _ :$ _ :$ a) <- t = return a

    constructFeatOpt feat args = constructFeatUnOpt feat args

    constructFeatUnOpt = constructFeatUnOptDefault
-}

