{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
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

module Feldspar.Option where



import qualified Prelude
import Control.Applicative (Applicative(..))
import Control.Monad

import Language.Syntactic

import Feldspar hiding (sugar,desugar,resugar)



data Option a = Option { isSome :: Data Bool, fromSome :: a }

instance Syntax a => Syntactic (Option a)
  where
    type Domain (Option a)   = FeldDomain
    type Internal (Option a) = (Bool, Internal a)
    desugar = desugar . desugarOption . fmap resugar
    sugar   = fmap resugar . sugarOption . sugar

instance Functor Option
  where
    fmap f opt = opt {fromSome = f (fromSome opt)}

instance Applicative Option
  where
    pure  = return
    (<*>) = ap

instance Monad Option
  where
    return = some
    a >>= f = b { isSome = isSome a ? isSome b $ false }
      where
        b = f (fromSome a)



-- | One-layer desugaring of 'Option'
desugarOption :: Type a => Option (Data a) -> Data (Bool,a)
desugarOption a = resugar (isSome a, fromSome a)

-- | One-layer sugaring of 'Option'
sugarOption :: Type a => Data (Bool,a) -> Option (Data a)
sugarOption (resugar -> (valid,a)) = Option valid a

some :: a -> Option a
some = Option true

none :: Syntax a => Option a
none = Option false (err "fromSome: none")

option :: Syntax b => b -> (a -> b) -> Option a -> b
option noneCase someCase opt = isSome opt
    ? someCase (fromSome opt)
    $ noneCase

oplus :: Syntax a => Option a -> Option a -> Option a
oplus a b = isSome a ? a $ b



--------------------------------------------------------------------------------
-- * Conditional choice operator
--------------------------------------------------------------------------------

-- http://zenzike.com/posts/2011-08-01-the-conditional-choice-operator

-- | Conditional choice operator. Can be used together with '<?' to write
-- guarded choices as follows:
--
-- > prog :: Data Index -> Data Index
-- > prog a
-- >     =  a+1 <? a==0
-- >     ?> a+2 <? a==1
-- >     ?> a+3 <? a==2
-- >     ?> a+4 <? a==3
-- >     ?> a+5
(?>) :: Data Bool -> a -> Option a
cond ?> a = Option (not cond) a

(<?) :: Syntax a => a -> Option a -> a
a <? b = option a id b

infixr 0 <?
infixr 0 ?>

