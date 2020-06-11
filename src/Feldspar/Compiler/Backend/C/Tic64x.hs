{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wall #-}

module Feldspar.Compiler.Backend.C.Tic64x (adaptTic64x) where

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Backend.C.Platforms (extend, tic64x)

-- This module does two major things:
--
-- 1) Replaces all single argument functions where the argument is of complex
--    type with a single named function. The renamer can't currently match on
--    that.
-- 2) Replaces complex typed "/=" and "bitCount" with an expression.
--
-- TODO: Extend the renamer to cope with #1.

-- | External interface for tic64x specific fixes.
adaptTic64x :: Options -> Module () -> Module ()
adaptTic64x opts m
 | "tic64x" == name (platform opts) = adaptTic64x' m
 | otherwise = m

-- | Internal interface for renaming.
adaptTic64x' :: Module () -> Module ()
adaptTic64x' (Module ents) = Module $ map adaptTic64xEnt ents

-- | Adapts entities.
adaptTic64xEnt :: Entity () -> Entity ()
adaptTic64xEnt p@Proc{..}
  | Just body <- procBody = p { procBody = Just $ adaptTic64xBlock body }
adaptTic64xEnt e             = e

-- | Adapts blocks.
adaptTic64xBlock :: Block () -> Block ()
adaptTic64xBlock (Block vs p) = Block (map adaptTic64xDecl vs) (adaptTic64xProg p)

-- | Adapts declarations.
adaptTic64xDecl :: Declaration () -> Declaration ()
adaptTic64xDecl (Declaration v (Just e)) = Declaration v (Just $ adaptTic64xExp e)
adaptTic64xDecl d                        = d

-- | Adapts programs.
adaptTic64xProg :: Program () -> Program ()
adaptTic64xProg e@Empty              = e
adaptTic64xProg c@Comment{}          = c
adaptTic64xProg (Assign lhs rhs)     = Assign (adaptTic64xExp lhs) (adaptTic64xExp rhs)
adaptTic64xProg (ProcedureCall n ps) = ProcedureCall n (map adaptTic64xParam ps)
adaptTic64xProg (Sequence ps)        = Sequence $ map adaptTic64xProg ps
adaptTic64xProg (Switch scrut alts)
   = Switch (adaptTic64xExp scrut) (map adaptTic64xAlt alts)
adaptTic64xProg (SeqLoop cond calc block)
  = SeqLoop (adaptTic64xExp cond) (adaptTic64xBlock calc) (adaptTic64xBlock block)
adaptTic64xProg (ParLoop p v e0 e1 e2 b)
  = ParLoop p v (adaptTic64xExp e0) (adaptTic64xExp e1) (adaptTic64xExp e2) (adaptTic64xBlock b)
adaptTic64xProg (BlockProgram b)     = BlockProgram $ adaptTic64xBlock b

-- | Adapts expressions.
adaptTic64xExp :: Expression () -> Expression ()
adaptTic64xExp v@VarExpr{}         = v
adaptTic64xExp (ArrayElem e es)    = ArrayElem (adaptTic64xExp e) $ map adaptTic64xExp es
adaptTic64xExp (StructField e s)   = StructField (adaptTic64xExp e) s
adaptTic64xExp c@ConstExpr{}       = c
adaptTic64xExp (FunctionCall (Function "/=" t) [arg1,arg2]) | isComplex (typeof arg1)
  = fun t "!" [fun t (extend tic64x "equal" $ typeof arg1) [arg1, arg2]]
adaptTic64xExp (FunctionCall (Function "bitCount" t) [arg]) | isComplex (typeof arg)
  = fun t "_dotpu4" [fun t "_bitc4" [arg], litI32 0x01010101]
adaptTic64xExp (FunctionCall f es)
  = FunctionCall (adaptTic64xFun argtype (length es) f) $ map adaptTic64xExp es
   where argtype = typeof $ head es
adaptTic64xExp (Cast t e)          = Cast t $ adaptTic64xExp e
adaptTic64xExp (AddrOf e)          = AddrOf $ adaptTic64xExp e
adaptTic64xExp s@SizeOf{}          = s
adaptTic64xExp (Deref e)           = Deref $ adaptTic64xExp e

-- | Adapts parameters.
adaptTic64xParam :: ActualParameter ()    -> ActualParameter ()
adaptTic64xParam (ValueParameter e) = ValueParameter $ adaptTic64xExp e
adaptTic64xParam p                  = p

-- | Adapts switch alternatives.
adaptTic64xAlt :: (Pattern (), Block ()) -> (Pattern (), Block ())
adaptTic64xAlt (p, b) = (p, adaptTic64xBlock b)

-- | Adapts functions that should be adapted Identity function on others.
adaptTic64xFun :: Type -> Int -> Function -> Function
adaptTic64xFun argtype args f@(Function _ t)
  | isComplex argtype
  , args == 1 -- TODO: This transformation looks dangerous.
  = Function (extend tic64x "creal" argtype) t
  | otherwise                           = f
