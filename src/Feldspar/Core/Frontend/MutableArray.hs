{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Feldspar.Core.Frontend.MutableArray
where

import Language.Syntactic

import Data.List (genericLength)
import Control.Monad (zipWithM_)

import Feldspar.Core.Types
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Loop
import Feldspar.Core.Constructs.MutableArray
import Feldspar.Core.Frontend.Mutable
import Feldspar.Core.Frontend.Literal (value)

-- | Create a new 'Mutable' Array and intialize all elements
newArr :: Type a => Data Length -> Data a -> M (Data (MArr a))
newArr = sugarSymC NewArr

-- | Create a new 'Mutable' Array but leave the elements un-initialized
newArr_ :: Type a => Data Length -> M (Data (MArr a))
newArr_ = sugarSymC NewArr_

-- | Create a new 'Mutable' Array and initialize with elements from the
-- list
newListArr :: forall a. Type a => [Data a] -> M (Data (MArr a))
newListArr xs = do arr <- newArr_ (value $ genericLength xs)
                   zipWithM_ (setArr arr . value) [0..] xs
                   return arr

-- | Extract the element at index
getArr :: Type a => Data (MArr a) -> Data Index -> M (Data a)
getArr = sugarSymC GetArr

-- | Replace the value at index
setArr :: Type a => Data (MArr a) -> Data Index -> Data a -> M ()
setArr = sugarSymC SetArr

-- | Modify the element at index
modifyArr :: Type a
          => Data (MArr a) -> Data Index -> (Data a -> Data a) -> M ()
modifyArr arr i f = getArr arr i >>= setArr arr i . f

-- | Query the length of the array
arrLength :: Type a => Data (MArr a) -> M (Data Length)
arrLength = sugarSymC ArrLength

-- | Modify all elements
mapArray :: Type a => (Data a -> Data a) -> Data (MArr a) -> M (Data (MArr a))
mapArray f arr = do
    len <- arrLength arr
    forArr len (flip (modifyArr arr) f)
    return arr

forArr :: Syntax a => Data Length -> (Data Index -> M a) -> M ()
forArr = sugarSymC For

-- | Swap two elements
swap :: Type a
     => Data (MArr a) -> Data Index -> Data Index -> M ()
swap a i1 i2 = do
    tmp <- getArr a i1
    getArr a i2 >>= setArr a i1
    setArr a i2 tmp

