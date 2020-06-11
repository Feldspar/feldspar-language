{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

-- | Type rewriting for Feldspar programs
module Feldspar.Compiler.CallConv
  ( rewriteType
  , buildHaskellType
  , buildCType
  ) where


import Language.Haskell.TH

import System.Plugins.MultiStage hiding (ref)

import Foreign.Ptr (Ptr)

import Feldspar (Syntactic(..))

-- | Normalize the type (expand type synonyms and type families)
rewriteType :: Type -> Q Type
rewriteType = applyTF ''Internal

haskellCC :: CallConv
haskellCC = CallConv { arg  = return
                     , res  = appT (conT ''IO) . return
                     }

feldsparCC :: CallConv
feldsparCC = CallConv { arg = ref . rep . return
                      , res = toIO . appT (conT ''Ptr) . rep . return
                      }
  where
    ref    = appT (conT ''Ref)
    rep    = appT (conT ''Rep)
    toIO t = appT (appT arrowT t) (appT (conT ''IO) (tupleT 0))

-- | Construct the corresponding Haskell type of a foreign Feldspar
-- function
--
-- > prog1 :: Data Index -> Vector1 Index
-- >
-- > sigD (mkName "h_prog1") $ loadFunType 'prog1 >>= rewriteType >>= buildHaskellType
--
-- becomes
--
-- > h_prog1 :: Index -> IO [Index]
--
buildHaskellType :: Type -> Q Type
buildHaskellType = buildType haskellCC

-- | Construct the corresponding C type of a compiled Feldspar function
--
-- > sigD (mkName "c_prog1_fun") $ loadFunType 'prog1 >>= rewriteType
--                                                    >>= buildCType
--
-- becomes
--
-- > c_prog1_fun :: Word32 -> Ptr (SA Word32) -> IO ()
--
buildCType :: Type -> Q Type
buildCType = buildType feldsparCC
