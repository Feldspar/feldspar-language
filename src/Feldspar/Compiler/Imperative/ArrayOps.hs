--
-- Copyright (c) 2020, ERICSSON AB
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
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

-- | Generation of type specific array management functions.
module Feldspar.Compiler.Imperative.ArrayOps (arrayOps) where

import Feldspar.Compiler.Backend.C.Options (Options)
import Feldspar.Compiler.Imperative.Frontend
        (litI32, fun, call, for, toBlock, mkIf, isShallow, variant,
         arrayFun, freeArrayE, lowerCopy, mkSequence, elemTyAwL)
import Feldspar.Compiler.Imperative.Representation
        (Module(..), Entity(..), Declaration(..), Program(..), Function(..),
         ParType(..), Block(..), ActualParameter(..), Expression(..),
         Variable(..), Type(..), ScalarType(..), HasType(..), Size(..),
         Signedness(..))
import Feldspar.Range (fullRange)

import Control.Monad.Writer (censor, runWriter, tell)
import Data.List (concatMap, isPrefixOf, nub)

-- | Main interface for adding needed array operations to a module.
arrayOps :: Options -> Module () -> Module ()
arrayOps opts (Module ents) = Module $ concatMap mkArrayOps dts ++ ents'
  where dts = filter (not . either isShallow isShallow) lrts
        (ents',lrts) = lower opts ents
        mkArrayOps (Left  t) = [mkInitArray opts t, mkFreeArray opts t]
        mkArrayOps (Right t) = [mkCopyArrayPos opts t, mkCopyArray opts t, mkInitCopyArray opts t]

-- | Copying an array to a given position in the destination
mkCopyArrayPos :: Options -> Type -> Entity ()
mkCopyArrayPos opts t = Proc name False [dstVar, dstLVar, srcVar, srcLVar, posVar] (typeof dstVar) (Just body)
  where name = variant "copyArrayPos" t
        body = Block decls prog
        decls = []
        ixVar = Variable intT "i"
        srcVar = Variable (ArrayType fullRange t) "src"
        srcLVar = Variable intT "srcLen"
        dstVar = Variable (ArrayType fullRange t) "dst"
        dstLVar = Variable intT "dstLen"
        posVar = Variable intT "pos"
        prog = Sequence
                 [ for Parallel ixVar zero (VarExpr srcLVar) one loopBody'
                 , call "return" [ValueParameter $ VarExpr dstVar]
                 ]
        loopBody' = Block lbDecls (Sequence lbProg)
        lbDecls = []
        lbProg = lowerCopy opts t lhs' [lhs', ArrayElem (VarExpr srcVar) [VarExpr ixVar]]
             where lhs' = ArrayElem (VarExpr dstVar) [fun intT "+" [VarExpr posVar, VarExpr ixVar]]

-- FIXME: Remove unused options argument.
-- | Copying an array to the beginning of another array
mkCopyArray :: Options -> Type -> Entity ()
mkCopyArray _ t = Proc name False [dstVar, dstLVar, srcVar, srcLVar] (typeof dstVar) (Just body)
  where name = variant "copyArray" t
        srcVar = Variable (ArrayType fullRange t) "src"
        srcLVar = Variable intT "srcLen"
        dstVar = Variable (ArrayType fullRange t) "dst"
        dstLVar = Variable intT "dstLen"
        body = Block decls prog
        decls = []
        prog = Sequence
               [ Assign (VarExpr dstVar)
                        (fun (ArrayType fullRange t)
                             (variant "copyArrayPos" t)
                             [VarExpr dstVar, VarExpr dstLVar, VarExpr srcVar, VarExpr srcLVar, zero])
               , call "return" [ValueParameter $ VarExpr dstVar]
               ]

-- FIXME: Remove unused options argument.
-- | Initializing and copying in a single operation
mkInitCopyArray :: Options -> Type -> Entity ()
mkInitCopyArray _ t = Proc name False [dstVar, dstLVar, srcVar, srcLVar] (typeof dstVar) (Just body)
  where name = variant "initCopyArray" t
        srcVar = Variable (ArrayType fullRange t) "src"
        srcLVar = Variable intT "srcLen"
        dstVar = Variable (ArrayType fullRange t) "dst"
        dstLVar = Variable intT "dstLen"
        body = Block decls prog
        decls = []
        prog = Sequence
               [ Assign (VarExpr dstVar) (arrayFun "initArray" [VarExpr dstVar, VarExpr dstLVar, VarExpr srcLVar])
               , Assign (VarExpr dstLVar) (VarExpr srcLVar)
               , Assign (VarExpr dstVar)
                        (fun (ArrayType fullRange t)
                             (variant "copyArrayPos" t)
                             [VarExpr dstVar, VarExpr dstLVar, VarExpr srcVar, VarExpr srcLVar, zero])
               , call "return" [ValueParameter $ VarExpr dstVar]
               ]

-- FIXME: Remove unused optoins argument.
-- | Initialize an array to a given length
mkInitArray :: Options -> Type -> Entity ()
mkInitArray _ t = Proc name False [dstVar, oldLen, newLen] (typeof dstVar) (Just body)
  where name = variant "initArray" t
        dstVar = Variable (ArrayType fullRange t) "dst"
        oldLen = Variable lengthT "oldLen"
        newLen = Variable lengthT "newLen"
        body = Block [] prog
        prog = Sequence [ mkIf (fun boolT "/=" [VarExpr oldLen, VarExpr newLen]) (toBlock setLength) Nothing
                        , call "return" [ValueParameter $ VarExpr dstVar]
                        ]
        setLength = Sequence [ mkIf (fun boolT "<" [VarExpr oldLen, VarExpr newLen]) (toBlock grow) (Just $ toBlock shrink)
                             ]
        grow = Sequence [ Assign (VarExpr dstVar) (fun arrT "resizeArray" [VarExpr dstVar, SizeOf t, VarExpr newLen])
                        , for Parallel ixVar (VarExpr oldLen) (VarExpr newLen) one (Block initBodyDecls initBody)
                        ]
        initBody = Sequence [Assign e (VarExpr $ nullVar t' i)
                            | ((e, t'), i) <- zip arrs [0 :: Int ..]]
        initBodyDecls = [Declaration (nullVar t' i) Nothing
                        | ((_, t'), i) <- zip arrs [0 :: Int ..]]
        shrink = Sequence [ for Parallel ixVar (VarExpr newLen) (VarExpr oldLen) one (toBlock freeBody)
                          , Assign (VarExpr dstVar) (fun arrT "resizeArray" [VarExpr dstVar, SizeOf t, VarExpr newLen])
                          ]
        freeBody = Sequence [freeArrayE e | (e,_) <- arrs]

        arrs = arrays (ArrayElem (VarExpr dstVar) [VarExpr ixVar]) t

        ixVar = Variable intT "i"
        nullVar t' n = Variable t' ("null_arr_" ++ show n)
        arrT = ArrayType fullRange t

-- FIXME: Remove unused options argument.
-- | Free an array
mkFreeArray :: Options -> Type -> Entity ()
mkFreeArray _ t = Proc name False [srcVar, srcLVar] VoidType (Just body)
  where name = variant "freeArray" t
        srcVar = Variable (ArrayType fullRange t) "src"
        srcLVar = Variable intT "srcLen"
        body = Block [] $ Sequence stms
        stms = [ for Parallel ixVar zero (VarExpr srcLVar) one loopBody'
               , call "freeArray" [ValueParameter $ VarExpr srcVar]
               ]
        ixVar = Variable intT "i"
        loopBody' = toBlock $ Sequence [freeArrayE e | (e,_) <- arrs]
        arrs = arrays (ArrayElem (VarExpr srcVar) [VarExpr ixVar]) t

-- | Type names
lengthT, intT, boolT :: Type
lengthT = 1 :# NumType Unsigned S32
intT    = 1 :# NumType Signed S32
boolT   = 1 :# BoolType

-- | Extract all arrays nested in a type
arrays :: Expression () -> Type -> [(Expression (), Type)]
arrays e t@(StructType _ [("buffer", ArrayType _ _),_]) = [(e,t)]
arrays e (NativeArray _ t) = [(e,t)]
arrays e (StructType _ fs) = concat [arrays (StructField e f) t | (f,t) <- fs]
arrays _ _                 = []

-- | Add types that will also need array operations
close :: Either Type Type -> [Either Type Type]
close et
  | Left  t <- et = map Left  $ t : go t
  | Right t <- et = map Right $ t : go t
  where go t | Just et' <- elemTyAwL t = t:go et'
        go (StructType _ fs) = concatMap (go . snd) fs
        go _                 = []

-- | Lower copy function and collect array op variants
lower :: Options -> [Entity ()] -> ([Entity ()], [Either Type Type])
lower opts es' = runWriter $ censor (nub . concatMap close) $ mapM lcEnt es'
  where lcEnt p@Proc{procBody = Just b} = do b' <- lcBlock b
                                             return p{procBody = Just b'}
        lcEnt e                         = return e

        lcBlock block = do body <- lcProg $ blockBody block
                           return block{blockBody = body}

        lcProg (Assign lhs' e@(FunctionCall (Function "copy" _) es))
          = do let t = typeof lhs'
                   ts = eTypesL e t
               tell $ map Right ts ++ map Left ts
               return $ mkSequence $ lowerCopy opts t lhs' es
        lcProg (Assign lhs' e@(FunctionCall (Function name _) _))
          | "initArray" `isPrefixOf` name
          = do tell $ map Left $ eTypes e $ typeof lhs'
               return $ Assign lhs' e
        lcProg (Sequence ps) = do ps' <- mapM lcProg ps
                                  return $ Sequence ps'
        lcProg (Switch e alts') = do alts'' <- mapM lcAlt alts'
                                     return $ Switch e alts''
        lcProg (SeqLoop e c b) = do c' <- lcBlock c
                                    b' <- lcBlock b
                                    return $ SeqLoop e c' b'
        lcProg (ParLoop p n s e i b) = do b' <- lcBlock b
                                          return $ ParLoop p n s e i b'
        lcProg (ProcedureCall f args)
               | "ivar_get_array" `isPrefixOf` f
               , [ValueParameter e, _, _] <- args
               = do let ts = eTypesL e $ typeof $ Deref e
                    tell $ map Left ts ++ map Right ts
                    return $ ProcedureCall f args
               | "ivar_put_array" `isPrefixOf` f
               , [_, ValueParameter e, _] <- args
               = do let ts = eTypesL e $ typeof $ Deref e
                    tell $ map Left ts ++ map Right ts
                    return $ ProcedureCall f args
        lcProg p = return p

        lcAlt (p, b) = do b' <- lcBlock b
                          return (p, b')

        eTypesL _ (StructType _ [("buffer", ArrayType _ t), _]) = [t]
        eTypesL _ (NativeArray _ t) = [t]
        eTypesL e t = error $ "ArrayOps.eTypesL: surprising array type "
                            ++ show t ++ " with rhs\n  " ++ show e

        eTypes _ (ArrayType _ t) = [t]
        eTypes _ (NativeArray _ t) = [t]
        eTypes e t = error $ "ArrayOps.eTypes: surprising array type "
                           ++ show t ++ " with rhs\n  " ++ show e

-- | Utilities
zero :: Expression ()
zero = litI32 0
one :: Expression ()
one  = litI32 1
