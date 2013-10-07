{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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

module Feldspar.Core.Frontend.Array
where

import Data.Patch

import Feldspar.Core.Types
import Feldspar.Core.Collection
import Feldspar.Core.Constructs
import Feldspar.Core.Constructs.Array
import Feldspar.Core.Frontend.Tuple ()

parallel :: Type a => Data Length -> (Data Index -> Data a) -> Data [a]
parallel = sugarSymF Parallel


sequential :: (Type a, Syntax s) =>
              Data Length -> s -> (Data Index -> s -> (Data a,s)) -> Data [a]
sequential = sugarSymF Sequential


append :: Type a => Data [a] -> Data [a] -> Data [a]
append = sugarSymF Append

getLength :: Type a => Data [a] -> Data Length
getLength = sugarSymF GetLength

-- | Change the length of the vector to the supplied value. If the supplied
-- length is greater than the old length, the new elements will have undefined
-- value.
setLength :: Type a => Data Length -> Data [a] -> Data [a]
setLength = sugarSymF SetLength

getIx :: Type a => Data [a] -> Data Index -> Data a
getIx = sugarSymF GetIx

setIx :: Type a => Data [a] -> Data Index -> Data a -> Data [a]
setIx = sugarSymF SetIx

type instance Elem      (Data [a]) = Data a
type instance CollIndex (Data [a]) = Data Index
type instance CollSize  (Data [a]) = Data Length

instance Type a => Indexed (Data [a])
  where
    (!) = getIx

instance Type a => Sized (Data [a])
  where
    collSize    = getLength
    setCollSize = setLength

instance (Type a, Type b) => CollMap (Data [a]) (Data [b])
  where
    collMap f arr = parallel (getLength arr) (f . getIx arr)

-- | Array patch
(|>) :: (Sized a, CollMap a a) =>
    Patch (CollSize a) (CollSize a) -> Patch (Elem a) (Elem a) -> Patch a a
(sizePatch |> elemPatch) a =
    collMap elemPatch $ setCollSize (sizePatch (collSize a)) a

