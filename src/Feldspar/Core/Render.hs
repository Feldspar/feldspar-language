{-# LANGUAGE KindSignatures #-}

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

module Feldspar.Core.Render
  ( stringTree
  -- , stringTreeMap
  , stringTreeExp
  , stringTreeASTF
  , showAST
  , drawAST
  , writeHtmlAST
  , render
  , StringTree
  , showDecorWith
  , drawDecorWith
  ) where

import qualified Data.Map.Strict as M
import Data.Tree
import Data.Tree.View

import Feldspar.Core.UntypedRepresentation (AUntypedFeld, stringTreeExp, stringTree)
import qualified Feldspar.Core.UntypedRepresentation as U
import Feldspar.Core.Reify
import Feldspar.Core.Representation (CBind(..))
import Feldspar.Core.Types (TypeF)
import Feldspar.Core.Middleend.FromTyped (untypeUnOpt, untypeDecor)
import Feldspar.Core.Interpretation (defaultFeldOpts)
import Feldspar.ValueInfo (ValueInfo)

stringTreeASTF :: TypeF a => ASTF d a -> Tree String
stringTreeASTF = U.stringTree . untypeUnOpt defaultFeldOpts

{-
stringTreeMap :: CSEMap -> [Tree String]
stringTreeMap m = map stBind $ map snd $ M.toList m
  where stBind (v,e) = Node (show v) [stringTreeExp e]
-}

class StringTree (a :: * -> *) where

-- Copied from Langage.Syntactic.Interpretation.Render

-- | Show a syntax tree using ASCII art
showAST :: (StringTree dom, TypeF a) => ASTF dom a -> String
showAST = showTree . stringTreeASTF
{-# INLINABLE showAST #-}

-- | Print a syntax tree using ASCII art
drawAST :: (StringTree dom, TypeF a) => ASTF dom a -> IO ()
drawAST = putStrLn . showAST
{-# INLINABLE drawAST #-}

writeHtmlAST :: (StringTree sym, TypeF a) => FilePath -> ASTF sym a -> IO ()
writeHtmlAST file = writeHtmlTree Nothing file
                  . fmap (\n -> NodeInfo InitiallyExpanded n "")
                  . stringTreeASTF
{-# INLINABLE writeHtmlAST #-}

showDecorWith :: (StringTree d, TypeF a) => (ValueInfo -> String) -> ASTF d a -> String
showDecorWith f = showTree . stringTreeExp g . untypeDecor defaultFeldOpts
  where g x = " in " ++ f x

drawDecorWith :: (StringTree d, TypeF a) => (ValueInfo -> String) -> ASTF d a -> IO ()
drawDecorWith f = putStrLn . showDecorWith f
