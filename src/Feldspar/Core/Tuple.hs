{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

module Feldspar.Core.Tuple where

class Tuply a where
  type Unpack a
  unpack :: a -> Unpack a

instance Tuply (a,b) where
  type Unpack (a,b) = (a, (b, ()))
  unpack (a,b) = (a, (b, ()))
  {-# INLINE unpack #-}

instance Tuply (a,b,c) where
  type Unpack (a,b,c) = (a,(b,(c,())))
  unpack (a,b,c) = (a,(b,(c,())))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d) where
  type Unpack (a,b,c,d) = (a,(b,(c,(d,()))))
  unpack (a,b,c,d) = (a,(b,(c,(d,()))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e) where
  type Unpack (a,b,c,d,e)
             = (a,(b,(c,(d,(e,())))))
  unpack (a,b,c,d,e)
             = (a,(b,(c,(d,(e,())))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f) where
  type Unpack (a,b,c,d,e,f)
             = (a,(b,(c,(d,(e,(f,()))))))
  unpack (a,b,c,d,e,f)
             = (a,(b,(c,(d,(e,(f,()))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g) where
  type Unpack (a,b,c,d,e,f,g)
             = (a,(b,(c,(d,(e,(f,(g,())))))))
  unpack (a,b,c,d,e,f,g)
             = (a,(b,(c,(d,(e,(f,(g,())))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h) where
  type Unpack (a,b,c,d,e,f,g,h)
             = (a,(b,(c,(d,(e,(f,(g,(h,()))))))))
  unpack (a,b,c,d,e,f,g,h)
             = (a,(b,(c,(d,(e,(f,(g,(h,()))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i) where
  type Unpack (a,b,c,d,e,f,g,h,i)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,())))))))))
  unpack (a,b,c,d,e,f,g,h,i)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,())))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j) where
  type Unpack (a,b,c,d,e,f,g,h,i,j)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,()))))))))))
  unpack (a,b,c,d,e,f,g,h,i,j)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,()))))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k) where
  type Unpack (a,b,c,d,e,f,g,h,i,j,k)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,())))))))))))
  unpack (a,b,c,d,e,f,g,h,i,j,k)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,())))))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l) where
  type Unpack (a,b,c,d,e,f,g,h,i,j,k,l)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,()))))))))))))
  unpack (a,b,c,d,e,f,g,h,i,j,k,l)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,()))))))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l,m) where
  type Unpack (a,b,c,d,e,f,g,h,i,j,k,l,m)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,())))))))))))))
  unpack (a,b,c,d,e,f,g,h,i,j,k,l,m)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,())))))))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l,m,n) where
  type Unpack (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,(n,()))))))))))))))
  unpack (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
             = (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,(n,()))))))))))))))
  {-# INLINE unpack #-}

instance Tuply (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) where
  type Unpack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
             = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, ())))))))))))))))
  unpack (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
             = (a, (b, (c, (d, (e, (f, (g, (h, (i, (j, (k, (l, (m, (n, (o, ())))))))))))))))
  {-# INLINE unpack #-}


class Sel1C a where
  type Sel1T a
  sel1w :: a -> Sel1T a

instance Sel1C (a,z) where
  type Sel1T (a,z) = a
  sel1w = fst
  {-# INLINE sel1w #-}

class Sel1C a => Sel2C a where
  type Sel2T a
  sel2w :: a -> Sel2T a

instance Sel1C t => Sel2C (a,t) where
  type Sel2T (a,t) = Sel1T t
  sel2w = sel1w . snd
  {-# INLINE sel2w #-}

class Sel2C a => Sel3C a where
  type Sel3T a
  sel3w :: a -> Sel3T a

instance Sel2C t => Sel3C (a,t) where
  type Sel3T (a,t) = Sel2T t
  sel3w = sel2w . snd
  {-# INLINE sel3w #-}

class Sel3C a => Sel4C a where
  type Sel4T a
  sel4w :: a -> Sel4T a

instance Sel3C t => Sel4C (a,t) where
  type Sel4T (a,t) = Sel3T t
  sel4w = sel3w . snd
  {-# INLINE sel4w #-}

class Sel4C a => Sel5C a where
  type Sel5T a
  sel5w :: a -> Sel5T a

instance Sel4C t => Sel5C (a,t) where
  type Sel5T (a,t) = Sel4T t
  sel5w = sel4w . snd
  {-# INLINE sel5w #-}

class Sel5C a => Sel6C a where
  type Sel6T a
  sel6w :: a -> Sel6T a

instance Sel5C t => Sel6C (a,t) where
  type Sel6T (a,t) = Sel5T t
  sel6w = sel5w . snd
  {-# INLINE sel6w #-}

class Sel6C a => Sel7C a where
  type Sel7T a
  sel7w :: a -> Sel7T a

instance Sel6C t => Sel7C (a,t) where
  type Sel7T (a,t) = Sel6T t
  sel7w = sel6w . snd
  {-# INLINE sel7w #-}

class Sel7C a => Sel8C a where
  type Sel8T a
  sel8w :: a -> Sel8T a

instance Sel7C t => Sel8C (a,t) where
  type Sel8T (a,t) = Sel7T t
  sel8w = sel7w . snd
  {-# INLINE sel8w #-}

class Sel8C a => Sel9C a where
  type Sel9T a
  sel9w :: a -> Sel9T a

instance Sel8C t => Sel9C (a,t) where
  type Sel9T (a,t) = Sel8T t
  sel9w = sel8w . snd
  {-# INLINE sel9w #-}

class Sel9C a => Sel10C a where
  type Sel10T a
  sel10w :: a -> Sel10T a

instance Sel9C t => Sel10C (a,t) where
  type Sel10T (a,t) = Sel9T t
  sel10w = sel9w . snd
  {-# INLINE sel10w #-}

class Sel10C a => Sel11C a where
  type Sel11T a
  sel11w :: a -> Sel11T a

instance Sel10C t => Sel11C (a,t) where
  type Sel11T (a,t) = Sel10T t
  sel11w = sel10w . snd
  {-# INLINE sel11w #-}

class Sel11C a => Sel12C a where
  type Sel12T a
  sel12w :: a -> Sel12T a

instance Sel11C t => Sel12C (a,t) where
  type Sel12T (a,t) = Sel11T t
  sel12w = sel11w . snd
  {-# INLINE sel12w #-}

class Sel12C a => Sel13C a where
  type Sel13T a
  sel13w :: a -> Sel13T a

instance Sel12C t => Sel13C (a,t) where
  type Sel13T (a,t) = Sel12T t
  sel13w = sel12w . snd
  {-# INLINE sel13w #-}

class Sel13C a => Sel14C a where
  type Sel14T a
  sel14w :: a -> Sel14T a

instance Sel13C t => Sel14C (a,t) where
  type Sel14T (a,t) = Sel13T t
  sel14w = sel13w . snd
  {-# INLINE sel14w #-}

class Sel14C a => Sel15C a where
  type Sel15T a
  sel15w :: a -> Sel15T a

instance Sel14C t => Sel15C (a,t) where
  type Sel15T (a,t) = Sel14T t
  sel15w = sel14w . snd
  {-# INLINE sel15w #-}

sel1 :: (Tuply w, Sel1C (Unpack w)) => w -> Sel1T (Unpack w)
{-# INLINE sel1 #-}
sel1 = sel1w . unpack

sel2 :: (Tuply w, Sel2C (Unpack w)) => w -> Sel2T (Unpack w)
{-# INLINE sel2 #-}
sel2 = sel2w . unpack

sel3 :: (Tuply w, Sel3C (Unpack w)) => w -> Sel3T (Unpack w)
{-# INLINE sel3 #-}
sel3 = sel3w . unpack

sel4 :: (Tuply w, Sel4C (Unpack w)) => w -> Sel4T (Unpack w)
{-# INLINE sel4 #-}
sel4 = sel4w . unpack

sel5 :: (Tuply w, Sel5C (Unpack w)) => w -> Sel5T (Unpack w)
{-# INLINE sel5 #-}
sel5 = sel5w . unpack

sel6 :: (Tuply w, Sel6C (Unpack w)) => w -> Sel6T (Unpack w)
{-# INLINE sel6 #-}
sel6 = sel6w . unpack

sel7 :: (Tuply w, Sel7C (Unpack w)) => w -> Sel7T (Unpack w)
{-# INLINE sel7 #-}
sel7 = sel7w . unpack

sel8 :: (Tuply w, Sel8C (Unpack w)) => w -> Sel8T (Unpack w)
{-# INLINE sel8 #-}
sel8 = sel8w . unpack

sel9 :: (Tuply w, Sel9C (Unpack w)) => w -> Sel9T (Unpack w)
{-# INLINE sel9 #-}
sel9 = sel9w . unpack

sel10 :: (Tuply w, Sel10C (Unpack w)) => w -> Sel10T (Unpack w)
{-# INLINE sel10 #-}
sel10 = sel10w . unpack

sel11 :: (Tuply w, Sel11C (Unpack w)) => w -> Sel11T (Unpack w)
{-# INLINE sel11 #-}
sel11 = sel11w . unpack

sel12 :: (Tuply w, Sel12C (Unpack w)) => w -> Sel12T (Unpack w)
{-# INLINE sel12 #-}
sel12 = sel12w . unpack

sel13 :: (Tuply w, Sel13C (Unpack w)) => w -> Sel13T (Unpack w)
{-# INLINE sel13 #-}
sel13 = sel13w . unpack

sel14 :: (Tuply w, Sel14C (Unpack w)) => w -> Sel14T (Unpack w)
{-# INLINE sel14 #-}
sel14 = sel14w . unpack

sel15 :: (Tuply w, Sel15C (Unpack w)) => w -> Sel15T (Unpack w)
{-# INLINE sel15 #-}
sel15 = sel15w . unpack
