{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Feldspar.Core.Constructs.Tuple
    ( module Language.Syntactic.Constructs.Tuple
    ) where

import Data.Tuple.Select

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder (CLambda)
import Language.Syntactic.Constructs.Tuple

import Feldspar.Core.Types
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Binding

instance Sharable Tuple

instance Cumulative Tuple

instance SizeProp (Tuple :|| Type)
  where
    sizeProp (C' Tup2) (a :* b :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        = (infoSize ia, infoSize ib)
    sizeProp (C' Tup3) (a :* b :* c :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          )
    sizeProp (C' Tup4) (a :* b :* c :* d :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          )
    sizeProp (C' Tup5) (a :* b :* c :* d :* e :* Nil)
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
    sizeProp (C' Tup6) (a :* b :* c :* d :* e :* g :* Nil)
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
    sizeProp (C' Tup7) (a :* b :* c :* d :* e :* g :* h :* Nil)
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

    sizeProp (C' Tup8) (a :* b :* c :* d :* e :* g :* h :* i :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          )

    sizeProp (C' Tup9) (a :* b :* c :* d :* e :* g :* h :* i :* j :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          )

    sizeProp (C' Tup10) (a :* b :* c :* d :* e :* g :* h :* i :* j :* k :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        , WrapFull ik <- k
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          , infoSize ik
          )

    sizeProp (C' Tup11) (a :* b :* c :* d :* e :* g :* h :* i :* j :* k :* l :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        , WrapFull ik <- k
        , WrapFull il <- l
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          , infoSize ik
          , infoSize il
          )

    sizeProp (C' Tup12) (a :* b :* c :* d :* e :* g :* h :* i :* j :* k :* l :* m :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        , WrapFull ik <- k
        , WrapFull il <- l
        , WrapFull im <- m
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          , infoSize ik
          , infoSize il
          , infoSize im
          )

    sizeProp (C' Tup13) (a :* b :* c :* d :* e :* g :* h :* i :* j :* k :* l :* m :* n :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        , WrapFull ik <- k
        , WrapFull il <- l
        , WrapFull im <- m
        , WrapFull in' <- n
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          , infoSize ik
          , infoSize il
          , infoSize im
          , infoSize in'
          )

    sizeProp (C' Tup14) (a :* b :* c :* d :* e :* g :* h :* i :* j :* k :* l :* m :* n :* o :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        , WrapFull ik <- k
        , WrapFull il <- l
        , WrapFull im <- m
        , WrapFull in' <- n
        , WrapFull io <- o
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          , infoSize ik
          , infoSize il
          , infoSize im
          , infoSize in'
          , infoSize io
          )

    sizeProp (C' Tup15) (a :* b :* c :* d :* e :* g :* h :* i :* j :* k :* l :* m :* n :* o :* p :* Nil)
        | WrapFull ia <- a
        , WrapFull ib <- b
        , WrapFull ic <- c
        , WrapFull id <- d
        , WrapFull ie <- e
        , WrapFull ig <- g
        , WrapFull ih <- h
        , WrapFull ii <- i
        , WrapFull ij <- j
        , WrapFull ik <- k
        , WrapFull il <- l
        , WrapFull im <- m
        , WrapFull in' <- n
        , WrapFull io <- o
        , WrapFull ip <- p
        = ( infoSize ia
          , infoSize ib
          , infoSize ic
          , infoSize id
          , infoSize ie
          , infoSize ig
          , infoSize ih
          , infoSize ii
          , infoSize ij
          , infoSize ik
          , infoSize il
          , infoSize im
          , infoSize in'
          , infoSize io
          , infoSize ip
          )

instance Sharable Select
  where
    sharable _ = False

instance Cumulative Select

sel1Size :: (Sel1' a ~ b) => TypeRep a -> Size a -> Size b
sel1Size Tup2Type{} = sel1
sel1Size Tup3Type{} = sel1
sel1Size Tup4Type{} = sel1
sel1Size Tup5Type{} = sel1
sel1Size Tup6Type{} = sel1
sel1Size Tup7Type{} = sel1
sel1Size Tup8Type{} = sel1
sel1Size Tup9Type{} = sel1
sel1Size Tup10Type{} = sel1
sel1Size Tup11Type{} = sel1
sel1Size Tup12Type{} = sel1
sel1Size Tup13Type{} = sel1
sel1Size Tup14Type{} = sel1
sel1Size Tup15Type{} = sel1

sel2Size :: (Sel2' a ~ b) => TypeRep a -> Size a -> Size b
sel2Size Tup2Type{} = sel2
sel2Size Tup3Type{} = sel2
sel2Size Tup4Type{} = sel2
sel2Size Tup5Type{} = sel2
sel2Size Tup6Type{} = sel2
sel2Size Tup7Type{} = sel2
sel2Size Tup8Type{} = sel2
sel2Size Tup9Type{} = sel2
sel2Size Tup10Type{} = sel2
sel2Size Tup11Type{} = sel2
sel2Size Tup12Type{} = sel2
sel2Size Tup13Type{} = sel2
sel2Size Tup14Type{} = sel2
sel2Size Tup15Type{} = sel2

sel3Size :: (Sel3' a ~ b) => TypeRep a -> Size a -> Size b
sel3Size Tup3Type{} = sel3
sel3Size Tup4Type{} = sel3
sel3Size Tup5Type{} = sel3
sel3Size Tup6Type{} = sel3
sel3Size Tup7Type{} = sel3
sel3Size Tup8Type{} = sel3
sel3Size Tup9Type{} = sel3
sel3Size Tup10Type{} = sel3
sel3Size Tup11Type{} = sel3
sel3Size Tup12Type{} = sel3
sel3Size Tup13Type{} = sel3
sel3Size Tup14Type{} = sel3
sel3Size Tup15Type{} = sel3

sel4Size :: (Sel4' a ~ b) => TypeRep a -> Size a -> Size b
sel4Size Tup4Type{} = sel4
sel4Size Tup5Type{} = sel4
sel4Size Tup6Type{} = sel4
sel4Size Tup7Type{} = sel4
sel4Size Tup8Type{} = sel4
sel4Size Tup9Type{} = sel4
sel4Size Tup10Type{} = sel4
sel4Size Tup11Type{} = sel4
sel4Size Tup12Type{} = sel4
sel4Size Tup13Type{} = sel4
sel4Size Tup14Type{} = sel4
sel4Size Tup15Type{} = sel4

sel5Size :: (Sel5' a ~ b) => TypeRep a -> Size a -> Size b
sel5Size Tup5Type{} = sel5
sel5Size Tup6Type{} = sel5
sel5Size Tup7Type{} = sel5
sel5Size Tup8Type{} = sel5
sel5Size Tup9Type{} = sel5
sel5Size Tup10Type{} = sel5
sel5Size Tup11Type{} = sel5
sel5Size Tup12Type{} = sel5
sel5Size Tup13Type{} = sel5
sel5Size Tup14Type{} = sel5
sel5Size Tup15Type{} = sel5

sel6Size :: (Sel6' a ~ b) => TypeRep a -> Size a -> Size b
sel6Size Tup6Type{} = sel6
sel6Size Tup7Type{} = sel6
sel6Size Tup8Type{} = sel6
sel6Size Tup9Type{} = sel6
sel6Size Tup10Type{} = sel6
sel6Size Tup11Type{} = sel6
sel6Size Tup12Type{} = sel6
sel6Size Tup13Type{} = sel6
sel6Size Tup14Type{} = sel6
sel6Size Tup15Type{} = sel6

sel7Size :: (Sel7' a ~ b) => TypeRep a -> Size a -> Size b
sel7Size Tup7Type{} = sel7
sel7Size Tup8Type{} = sel7
sel7Size Tup9Type{} = sel7
sel7Size Tup10Type{} = sel7
sel7Size Tup11Type{} = sel7
sel7Size Tup12Type{} = sel7
sel7Size Tup13Type{} = sel7
sel7Size Tup14Type{} = sel7
sel7Size Tup15Type{} = sel7

sel8Size :: (Sel8' a ~ b) => TypeRep a -> Size a -> Size b
sel8Size Tup8Type{} = sel8
sel8Size Tup9Type{} = sel8
sel8Size Tup10Type{} = sel8
sel8Size Tup11Type{} = sel8
sel8Size Tup12Type{} = sel8
sel8Size Tup13Type{} = sel8
sel8Size Tup14Type{} = sel8
sel8Size Tup15Type{} = sel8

sel9Size :: (Sel9' a ~ b) => TypeRep a -> Size a -> Size b
sel9Size Tup9Type{} = sel9
sel9Size Tup10Type{} = sel9
sel9Size Tup11Type{} = sel9
sel9Size Tup12Type{} = sel9
sel9Size Tup13Type{} = sel9
sel9Size Tup14Type{} = sel9
sel9Size Tup15Type{} = sel9

sel10Size :: (Sel10' a ~ b) => TypeRep a -> Size a -> Size b
sel10Size Tup10Type{} = sel10
sel10Size Tup11Type{} = sel10
sel10Size Tup12Type{} = sel10
sel10Size Tup13Type{} = sel10
sel10Size Tup14Type{} = sel10
sel10Size Tup15Type{} = sel10

sel11Size :: (Sel11' a ~ b) => TypeRep a -> Size a -> Size b
sel11Size Tup11Type{} = sel11
sel11Size Tup12Type{} = sel11
sel11Size Tup13Type{} = sel11
sel11Size Tup14Type{} = sel11
sel11Size Tup15Type{} = sel11

sel12Size :: (Sel12' a ~ b) => TypeRep a -> Size a -> Size b
sel12Size Tup12Type{} = sel12
sel12Size Tup13Type{} = sel12
sel12Size Tup14Type{} = sel12
sel12Size Tup15Type{} = sel12

sel13Size :: (Sel13' a ~ b) => TypeRep a -> Size a -> Size b
sel13Size Tup13Type{} = sel13
sel13Size Tup14Type{} = sel13
sel13Size Tup15Type{} = sel13

sel14Size :: (Sel14' a ~ b) => TypeRep a -> Size a -> Size b
sel14Size Tup14Type{} = sel14
sel14Size Tup15Type{} = sel14

sel15Size :: (Sel15' a ~ b) => TypeRep a -> Size a -> Size b
sel15Size Tup15Type{} = sel15

instance SizeProp (Select :|| Type)
  where
    sizeProp (C' Sel1) (WrapFull ia :* Nil) =
        sel1Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel2) (WrapFull ia :* Nil) =
        sel2Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel3) (WrapFull ia :* Nil) =
        sel3Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel4) (WrapFull ia :* Nil) =
        sel4Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel5) (WrapFull ia :* Nil) =
        sel5Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel6) (WrapFull ia :* Nil) =
        sel6Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel7) (WrapFull ia :* Nil) =
        sel7Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel8) (WrapFull ia :* Nil) =
        sel8Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel9) (WrapFull ia :* Nil) =
        sel9Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel10) (WrapFull ia :* Nil) =
        sel10Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel11) (WrapFull ia :* Nil) =
        sel11Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel12) (WrapFull ia :* Nil) =
        sel12Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel13) (WrapFull ia :* Nil) =
        sel13Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel14) (WrapFull ia :* Nil) =
        sel14Size (infoType ia) (infoSize ia)
    sizeProp (C' Sel15) (WrapFull ia :* Nil) =
        sel15Size (infoType ia) (infoSize ia)

-- | Compute a witness that a symbol and an expression have the same result type
tupEq :: Type (DenResult a) =>
    sym a -> ASTF (Decor Info dom) b -> Maybe (TypeEq (DenResult a) b)
tupEq _ b = typeEq typeRep (infoType $ getInfo b)

instance
    ( (Tuple  :|| Type) :<: dom
    , (Select :|| Type) :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize (Tuple :|| Type) dom
  where
    constructFeatOpt _ (C' tup@Tup2) (s1 :* s2 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , alphaEq a b
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup3) (s1 :* s2 :* s3 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , alphaEq a b
        , alphaEq a c
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup4) (s1 :* s2 :* s3 :* s4 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup5) (s1 :* s2 :* s3 :* s4 :* s5 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup6) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup7) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup8) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup9) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup10) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* s10 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , (prjF -> Just (C' Sel10)) :$ j <- s10
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , alphaEq a j
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup11) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* s10 :* s11 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , (prjF -> Just (C' Sel10)) :$ j <- s10
        , (prjF -> Just (C' Sel11)) :$ k <- s11
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , alphaEq a j
        , alphaEq a k
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup12) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* s10 :* s11 :* s12 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , (prjF -> Just (C' Sel10)) :$ j <- s10
        , (prjF -> Just (C' Sel11)) :$ k <- s11
        , (prjF -> Just (C' Sel12)) :$ l <- s12
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , alphaEq a j
        , alphaEq a k
        , alphaEq a l
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup13) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* s10 :* s11 :* s12 :* s13 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , (prjF -> Just (C' Sel10)) :$ j <- s10
        , (prjF -> Just (C' Sel11)) :$ k <- s11
        , (prjF -> Just (C' Sel12)) :$ l <- s12
        , (prjF -> Just (C' Sel13)) :$ m <- s13
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , alphaEq a j
        , alphaEq a k
        , alphaEq a l
        , alphaEq a m
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup14) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* s10 :* s11 :* s12 :* s13 :* s14 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , (prjF -> Just (C' Sel10)) :$ j <- s10
        , (prjF -> Just (C' Sel11)) :$ k <- s11
        , (prjF -> Just (C' Sel12)) :$ l <- s12
        , (prjF -> Just (C' Sel13)) :$ m <- s13
        , (prjF -> Just (C' Sel14)) :$ n <- s14
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , alphaEq a j
        , alphaEq a k
        , alphaEq a l
        , alphaEq a m
        , alphaEq a n
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt _ (C' tup@Tup15) (s1 :* s2 :* s3 :* s4 :* s5 :* s6 :* s7 :* s8 :* s9 :* s10 :* s11 :* s12 :* s13 :* s14 :* s15 :* Nil)
        | (prjF -> Just (C' Sel1)) :$ a <- s1
        , (prjF -> Just (C' Sel2)) :$ b <- s2
        , (prjF -> Just (C' Sel3)) :$ c <- s3
        , (prjF -> Just (C' Sel4)) :$ d <- s4
        , (prjF -> Just (C' Sel5)) :$ e <- s5
        , (prjF -> Just (C' Sel6)) :$ f <- s6
        , (prjF -> Just (C' Sel7)) :$ g <- s7
        , (prjF -> Just (C' Sel8)) :$ h <- s8
        , (prjF -> Just (C' Sel9)) :$ i <- s9
        , (prjF -> Just (C' Sel10)) :$ j <- s10
        , (prjF -> Just (C' Sel11)) :$ k <- s11
        , (prjF -> Just (C' Sel12)) :$ l <- s12
        , (prjF -> Just (C' Sel13)) :$ m <- s13
        , (prjF -> Just (C' Sel14)) :$ n <- s14
        , (prjF -> Just (C' Sel15)) :$ o <- s15
        , alphaEq a b
        , alphaEq a c
        , alphaEq a d
        , alphaEq a e
        , alphaEq a f
        , alphaEq a g
        , alphaEq a h
        , alphaEq a i
        , alphaEq a j
        , alphaEq a k
        , alphaEq a l
        , alphaEq a m
        , alphaEq a n
        , alphaEq a o
        , Just TypeEq <- tupEq tup a
        = return a

    constructFeatOpt opts feat args = constructFeatUnOpt opts feat args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x


instance
    ( (Select :|| Type) :<: dom
    , CLambda Type :<: dom
    , (Tuple  :|| Type) :<: dom
    , Let :<: dom
    , (Variable :|| Type) :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize (Select :|| Type) dom
  where
    constructFeatOpt opts s@(C' Sel1) (t :* Nil)
        | ((prjF -> Just (C' Tup2)) :$ a :$ _) <- t                          = return a
        | ((prjF -> Just (C' Tup3)) :$ a :$ _ :$ _) <- t                     = return a
        | ((prjF -> Just (C' Tup4)) :$ a :$ _ :$ _ :$ _) <- t                = return a
        | ((prjF -> Just (C' Tup5)) :$ a :$ _ :$ _ :$ _ :$ _) <- t           = return a
        | ((prjF -> Just (C' Tup6)) :$ a :$ _ :$ _ :$ _ :$ _ :$ _) <- t      = return a
        | ((prjF -> Just (C' Tup7)) :$ a :$ _ :$ _ :$ _ :$ _ :$ _ :$ _) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)


    constructFeatOpt opts s@(C' Sel2) (t :* Nil)
        | ((prjF -> Just (C' Tup2)) :$ _ :$ a) <- t                          = return a
        | ((prjF -> Just (C' Tup3)) :$ _ :$ a :$ _) <- t                     = return a
        | ((prjF -> Just (C' Tup4)) :$ _ :$ a :$ _ :$ _) <- t                = return a
        | ((prjF -> Just (C' Tup5)) :$ _ :$ a :$ _ :$ _ :$ _) <- t           = return a
        | ((prjF -> Just (C' Tup6)) :$ _ :$ a :$ _ :$ _ :$ _ :$ _) <- t      = return a
        | ((prjF -> Just (C' Tup7)) :$ _ :$ a :$ _ :$ _ :$ _ :$ _ :$ _) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)

    constructFeatOpt opts s@(C' Sel3) (t :* Nil)
        | ((prjF -> Just (C' Tup3)) :$ _ :$ _ :$ a) <- t                     = return a
        | ((prjF -> Just (C' Tup4)) :$ _ :$ _ :$ a :$ _) <- t                = return a
        | ((prjF -> Just (C' Tup5)) :$ _ :$ _ :$ a :$ _ :$ _) <- t           = return a
        | ((prjF -> Just (C' Tup6)) :$ _ :$ _ :$ a :$ _ :$ _ :$ _) <- t      = return a
        | ((prjF -> Just (C' Tup7)) :$ _ :$ _ :$ a :$ _ :$ _ :$ _ :$ _) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)

    constructFeatOpt opts s@(C' Sel4) (t :* Nil)
        | ((prjF -> Just (C' Tup4)) :$ _ :$ _ :$ _ :$ a) <- t                = return a
        | ((prjF -> Just (C' Tup5)) :$ _ :$ _ :$ _ :$ a :$ _) <- t           = return a
        | ((prjF -> Just (C' Tup6)) :$ _ :$ _ :$ _ :$ a :$ _ :$ _) <- t      = return a
        | ((prjF -> Just (C' Tup7)) :$ _ :$ _ :$ _ :$ a :$ _ :$ _ :$ _) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)

    constructFeatOpt opts s@(C' Sel5) (t :* Nil)
        | ((prjF -> Just (C' Tup5)) :$ _ :$ _ :$ _ :$ _ :$ a) <- t           = return a
        | ((prjF -> Just (C' Tup6)) :$ _ :$ _ :$ _ :$ _ :$ a :$ _) <- t      = return a
        | ((prjF -> Just (C' Tup7)) :$ _ :$ _ :$ _ :$ _ :$ a :$ _ :$ _) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)

    constructFeatOpt opts s@(C' Sel6) (t :* Nil)
        | ((prjF -> Just (C' Tup6)) :$ _ :$ _ :$ _ :$ _ :$ _ :$ a) <- t      = return a
        | ((prjF -> Just (C' Tup7)) :$ _ :$ _ :$ _ :$ _ :$ _ :$ a :$ _) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)


    constructFeatOpt opts s@(C' Sel7) (t :* Nil)
        | ((prjF -> Just (C' Tup7)) :$ _ :$ _ :$ _ :$ _ :$ _ :$ _ :$ a) <- t = return a
        | ((prj  -> Just Let) :$ a :$ (lam :$ b)) <- t
        , (Just v@(SubConstr2 (Lambda {}))) <- prjLambda lam
         = do s' <- constructFeatOpt opts s (b :* Nil)
              b' <- constructFeatOpt opts (reuseCLambda v) (s' :* Nil)
              constructFeatOpt opts Let (a :* b' :* Nil)

    constructFeatOpt opts feat args = constructFeatUnOpt opts feat args

    constructFeatUnOpt opts x@(C' _) = constructFeatUnOptDefault opts x

