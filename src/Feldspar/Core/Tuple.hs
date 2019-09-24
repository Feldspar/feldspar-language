{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

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


sel1 :: (Tuply w, Unpack w ~ (a,z)) => w -> a
{-# INLINE sel1 #-}
sel1 = fst . unpack

sel2 :: (Tuply w, Unpack w ~ (a,(b,z))) => w -> b
{-# INLINE sel2 #-}
sel2 = fst . snd . unpack

sel3 :: (Tuply w, Unpack w ~ (a,(b,(c,z)))) => w -> c
{-# INLINE sel3 #-}
sel3 = fst . snd . snd . unpack

sel4 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,z))))) => w -> d
{-# INLINE sel4 #-}
sel4 = fst . snd . snd . snd . unpack

sel5 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,z)))))) => w -> e
{-# INLINE sel5 #-}
sel5 = fst . snd . snd . snd . snd . unpack

sel6 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,z))))))) => w -> f
{-# INLINE sel6 #-}
sel6 = fst . snd . snd . snd . snd . snd . unpack

sel7 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,(g,z)))))))) => w -> g
{-# INLINE sel7 #-}
sel7 = fst . snd . snd . snd . snd . snd . snd . unpack

sel8 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,z))))))))) => w -> h
{-# INLINE sel8 #-}
sel8 = fst . snd . snd . snd . snd . snd . snd . snd . unpack

sel9 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,z)))))))))) => w -> i
{-# INLINE sel9 #-}
sel9 = fst . snd . snd . snd . snd . snd . snd . snd . snd . unpack

sel10 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,z))))))))))
         ) => w -> j
{-# INLINE sel10 #-}
sel10 = fst . snd . snd . snd . snd . snd . snd . snd . snd . snd . unpack

sel11 :: (Tuply w, Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,z)))))))))))
         ) => w -> k
{-# INLINE sel11 #-}
sel11 = fst . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd . unpack

sel12 :: (Tuply w,
          Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,z))))))))))))
         ) => w -> l
{-# INLINE sel12 #-}
sel12 = fst . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd
            . unpack

sel13 :: (Tuply w,
          Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,z)))))))))))))
         ) => w -> m
{-# INLINE sel13 #-}
sel13 = fst . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd
            . snd . unpack

sel14 :: (Tuply w,
          Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,(n,z))))))))))))))
         ) => w -> n
{-# INLINE sel14 #-}
sel14 = fst . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd
            . snd . snd . unpack

sel15 :: (Tuply w,
          Unpack w ~ (a,(b,(c,(d,(e,(f,(g,(h,(i,(j,(k,(l,(m,(n,(o,z)))))))))))))))
         ) => w -> o
{-# INLINE sel15 #-}
sel15 = fst . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd . snd
            . snd . snd . snd . unpack


