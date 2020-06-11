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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
-- FIXME: Partial functions, eliminate.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | This module provides functions for translating from the 'UntypedFeld'
-- representation (\"Feldspar core\") to imperative code (from
-- "Feldspar.Compiler.Imperative.Representation").
--
-- Compilation is done as follows:
--
-- * The user calls one of the @fromCoreX@ functions
--
-- * @fromCoreX@ calls 'compileProgTop' which recurively deals with top-level
--   lambdas and let bindings.
--
-- * When 'compileProgTop' gets to the body (the first non-lambda, non-let
--   node), it calls 'compileProg' to generate code that writes the result to
--   the variable \"out\".
--
-- * The body of the code is generated using mutual recursion between
--   'compileProg' and 'compileExpr'. The former handles constructs whose
--   result cannot be represented as a non-variable 'Expression' (e.g. a
--   conditional), and the latter handles all other constructs (e.g. primitive
--   functions).

module Feldspar.Compiler.Imperative.FromCore
  ( fromCoreUT
  , fromCoreExp
  ) where

import Data.Char (toLower)
import Data.List (find, isPrefixOf, nub)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, mapMaybe)

import Control.Monad.RWS

import Feldspar.Core.Frontend (reifyFeld, Syntactic)
import Feldspar.Core.UntypedRepresentation
         ( VarId(..), UntypedFeld, Term(..), Lit(..)
         , UntypedFeldF(App, LetFun), Fork(..)
         )
import qualified Feldspar.Core.UntypedRepresentation as Ut
import Feldspar.Core.Middleend.FromTyped
import Feldspar.Range (fullRange, upperBound)

import Feldspar.Compiler.Backend.C.MachineLowering
import Feldspar.Compiler.Backend.C.Options (Options(..))
import Feldspar.Compiler.Backend.C.Platforms (c99, extend)
import Feldspar.Compiler.Imperative.FromCore.Interpretation
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.Representation

--------------------------------------------------------------------------------
-- * Top-level translation functions
--------------------------------------------------------------------------------

{-

Fast returns
------------

Fast returns really means single return value and that value fits in a
register--thus they are platform dependent. This is why we have to use
compileType since that can make configuration specific choices about
data layout.

The user is free to ask for fast returns (by setting 'useNativeReturns' to True),
but we might not be able to comply.

-}

-- | Get the generated core for an 'UntypedFeld' expression. The result is the
-- generated code and the next available variable identifier.
fromCoreUT
    :: Options
    -> String       -- ^ Name of the generated function
    -> UntypedFeld  -- ^ Expression to generate code for
    -> (Module (), VarId)
fromCoreUT opt funname uast = (Module defs, maxVar')
  where
    maxVar  = succ $ maximum $ map Ut.varNum $ Ut.allVars uast
    fastRet = useNativeReturns opt && canFastReturn (compileType opt (typeof uast))

    (outParam,maxVar',results) = runRWS (compileProgTop uast) (initEnv opt) maxVar

    decls      = decl results
    formals    = params results ++ outs
    post       = epilogue results ++ returns
    Block ds p = block results
    outDecl    = Declaration outParam Nothing
    paramTypes = getTypeDefs $ map (`Declaration` Nothing) formals
    defs       = nub (def results ++ paramTypes) ++ topProc

    (rtype, outs, ds', returns)
     | fastRet   = ( typeof outParam, [],  outDecl:ds ++ decls
                   , [call "return" [ValueParameter $ varToExpr outParam]])
     | otherwise = ( VoidType, [outParam],         ds ++ decls, [])

    topProc    = [Proc funname False formals rtype $ Just (Block ds' (Sequence mainProg))]

    mainProg
     | Just _ <- find isTask $ def results
     = call "taskpool_init" [four,four,four] : p : call "taskpool_shutdown" [] : post
     | otherwise = p:post
      where
        four = ValueParameter $ ConstExpr $ IntConst 4 $ NumType Ut.Unsigned Ut.S32

    isTask Proc{..}   = "task_core" `isPrefixOf` procName
    isTask _          = False

-- | Get the generated core for a program and an expression that contains the output. The components
-- of the result are as follows, in order:
--
-- * A list of extra entities needed by the program
-- * A list of declarations needed by the program
-- * The actual program
-- * An expression that contains the result
-- * A list of epilogue programs, for freeing memory, etc.
fromCoreExp :: MonadState VarId m
            => Syntactic a
            => Options
            -> Map.Map VarId String
            -> a
            -> m ([Entity ()], [Declaration ()], Program (), Expression (), [Program ()])
fromCoreExp opt aliases prog = do
    let uast = untype (frontendOpts opt) $ reifyFeld prog
        mkAlias (Ut.Var i t _) = do
          n <- Map.lookup i aliases
          return (i, varToExpr $ Variable (compileType opt t) n)
        as = mapMaybe mkAlias $ Ut.fv uast
    s <- get
    let (exp', s', results) = runRWS (compileExpr uast) (CodeEnv as False opt) s
    put s'
    unless (null $ params results) $ error "fromCoreExp: unexpected params"
    let x = getPlatformRenames opt
        Block ls p = block results
    return ( renameEnt  opt x <$> def results
           , renameDecl     x <$> (ls ++ decl results)
           , renameProg opt x p
           , renameExp x exp'
           , renameProg opt x <$> epilogue results
           )

-- | Generate code for an expression that may have top-level lambdas and let
-- bindings. The returned variable holds the result of the generated code.
compileProgTop :: Ut.UntypedFeld -> CodeWriter (Variable ())
compileProgTop (In (Ut.Lambda (Ut.Var v ta _) body)) = do
  opt <- asks backendOpts
  let typ = compileType opt ta
      (arg,arge) | StructType{} <- typ = (mkPointer typ v, Deref $ varToExpr arg)
                 | otherwise           = (mkVariable typ v, varToExpr arg)
  tell $ mempty {params=[arg]}
  withAlias v arge $
     compileProgTop body
compileProgTop (In (Ut.App Ut.Let _ [In (Ut.Literal l), In (Ut.Lambda (Ut.Var v _ _) body)]))
  | representableType l
  = do opt <- asks backendOpts
       let var = mkVariable (typeof c) v -- Note [Precise size information]
           c   = literalConst opt l
       tellDef [ValueDef var c]
       withAlias v (varToExpr var) $
         compileProgTop body
compileProgTop a = do
  opt <- asks backendOpts
  let outType' = compileType opt (typeof a)
      fastRet  = useNativeReturns opt && canFastReturn outType'
      (outType, outLoc)
       | fastRet   = (outType', varToExpr outParam)
       | otherwise = (1 :# Pointer outType', Deref $ varToExpr outParam)
      outParam   = Variable outType "out"
  compileProg (Just outLoc) a
  return outParam



--------------------------------------------------------------------------------
-- * compileType
--------------------------------------------------------------------------------

-- | Compile a type representation. The conversion is platform-dependent, which
-- is why the function takes and 'Options' argument.
compileType :: Options -> Ut.Type -> Type
compileType _   (n Ut.:# Ut.BoolType)      = n :# BoolType
compileType _   (n Ut.:# Ut.BitType)       = n :# BitType
compileType _   (n Ut.:# Ut.IntType s sz)  = n :# NumType s sz
compileType _   (n Ut.:# Ut.FloatType)     = n :# FloatType
compileType _   (n Ut.:# Ut.DoubleType)    = n :# DoubleType
compileType opt (n Ut.:# Ut.ComplexType t) = n :# ComplexType (compileType opt t)
compileType _   (Ut.TupType [])        = VoidType
compileType opt (Ut.TupType ts)        = mkStructType
    [("member" ++ show n, compileType opt t) | (n,t) <- zip [1 :: Int ..] ts]
compileType opt (Ut.MutType a)         = compileType opt a
compileType opt (Ut.RefType a)         = compileType opt a
compileType opt (Ut.ArrayType rs a)
 | useNativeArrays opt = NativeArray (Just $ upperBound rs) $ compileType opt a
 | otherwise           = mkAwLType rs $ compileType opt a
compileType opt (Ut.MArrType rs a)
 | useNativeArrays opt = NativeArray (Just $ upperBound rs) $ compileType opt a
 | otherwise           = mkAwLType rs $ compileType opt a
compileType opt (Ut.ParType a)         = compileType opt a
compileType opt (Ut.ElementsType a)
 | useNativeArrays opt = NativeArray Nothing $ compileType opt a
 | otherwise           = mkAwLType fullRange $ compileType opt a
compileType opt (Ut.IVarType a)        = IVarType $ compileType opt a
compileType opt (Ut.FunType _ b)       = compileType opt b
compileType opt (Ut.FValType a)        = IVarType $ compileType opt a

--------------------------------------------------------------------------------
-- * compileProg
--------------------------------------------------------------------------------

-- 'compileProg' should handle constructs whose result cannot be represented as
-- a non-variable 'Expression' (e.g. a conditional), and delegate all other
-- constructs to 'compileExpr'. Delegation is done by calling 'compileExprLoc'.

-- | Compile an expression and put the result in the given location
compileProg :: Location -> Ut.UntypedFeld -> CodeWriter ()
-- Array
compileProg (Just loc) (In (App Ut.Parallel _ [len, In (Ut.Lambda (Ut.Var v ta _) ixf)])) = do
   opts <- asks backendOpts
   let ix = mkVar (compileType opts ta) v
   len' <- mkLength len ta
   (ptyp, b) <- case ixf of
          In (App (Ut.Call Loop n) _ vs) -> do
            vs' <- mapM compileExpr vs
            let args  = map (ValueParameter . varToExpr) $ nub $ map varExpr vs' ++ fv loc
            return (TaskParallel, toBlock $ ProcedureCall n args)
          _                              -> do
            b' <- confiscateBlock $ compileProg (Just $ mkArrayElem loc [ix]) ixf
            return (Parallel, snd b')
   tellProg [initArray (Just loc) len']
   tellProg [for ptyp (varExpr ix) (litI32 0) len' (litI32 1) b]
compileProg loc (In (App Ut.Sequential _ [len, init', In (Ut.Lambda (Ut.Var v tix _) ixf1)]))
   | In (Ut.Lambda (Ut.Var s tst _) l) <- ixf1
   , (bs, In (Ut.App Ut.Tup _ [In (Ut.Variable t1), In (Ut.Variable t2)])) <- collectLetBinders l
   , not $ null bs
   , (e, step) <- last bs
   , t1 == e
   , t2 == e
   = do
        opts <- asks backendOpts
        blocks <- mapM (confiscateBlock . compileBind) (init bs)
        let (dss, lets) = unzip $ map (\(_, Block ds (Sequence body)) -> (ds, body)) blocks
        let ix = mkVar (compileType opts tix) v
        len' <- mkLength len tix
        st1 <- freshVar opts "st" tst
        let st = mkPointer (compileType opts tst) s
            st_val = Deref $ varToExpr st
        declareAlias st
        (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s st_val $ compileProg (mkArrayElem <$> loc <*> pure [ix]) step
        withAlias s st_val $ compileProg (Just st1) init'
        tellProg [ Assign (varToExpr st) (AddrOf st1)
                 , initArray loc len']
        tellProg [toProg $ Block (concat dss ++ ds) $
                  for Sequential (varExpr ix) (litI32 0) len' (litI32 1) $
                               toBlock $ Sequence (concat lets ++ body ++ maybe [] (\arr -> [Assign (varToExpr st) $ AddrOf (mkArrayElem arr [ix])]) loc)]
compileProg loc (In (App Ut.Sequential _ [len, st, In (Ut.Lambda (Ut.Var v t _) (In (Ut.Lambda (Ut.Var s _ _) step)))]))
  = do
       opts <- asks backendOpts
       let tr' = typeof step
       let ix = mkVar (compileType opts t) v
       len' <- mkLength len t
       tmp  <- freshVar opts "seq" tr'
       (_, Block ds (Sequence body)) <- confiscateBlock $ withAlias s (StructField tmp "member2") $ compileProg (Just tmp) step
       tellProg [initArray loc len']
       compileProg (Just $ StructField tmp "member2") st
       tellProg [toProg $ Block ds $
                 for Sequential (varExpr ix) (litI32 0) len' (litI32 1) $ toBlock $
                   Sequence $ body ++
                     [copyProg (mkArrayElem <$> loc <*> pure [ix]) [StructField tmp "member1"]
                     ]]
compileProg loc (In (App Ut.Append _ [a, b])) = do
   a' <- compileExpr a
   b' <- compileExpr b
   tellProg [copyProg loc [a', b']]
compileProg loc (In (App Ut.SetIx _ [arr, i, a])) = do
   compileProg loc arr
   i' <- compileExpr i
   compileProg (mkArrayElem <$> loc <*> pure [i']) a
compileProg loc (In (App Ut.GetIx _ [arr, i])) = do
   a' <- compileExpr arr
   i' <- compileExpr i
   let el = mkArrayElem a' [i']
   if isAwLType $ typeof el
      then shallowAssign loc el
      else assign loc el
compileProg loc (In (App Ut.SetLength _ [len, arr])) = do
   len' <- compileExpr len
   compileProg loc arr
   tellProg [initArray loc len']
-- Binding
compileProg _ e@(In Ut.Lambda{})
  = error ("Can only compile top-level lambda: " ++ show e)
compileProg loc (In (Ut.App Ut.Let _ [a, In (Ut.Lambda (Ut.Var v ta _) body)])) = do
   e <- compileLet a ta v
   withAlias v e $ compileProg loc body
-- Bits
-- Complex
-- Condition
compileProg loc (In (App Ut.Condition _ [cond, tHEN, eLSE])) =
   mkBranch loc cond tHEN $ Just eLSE
compileProg loc (In (App Ut.ConditionM _ [cond, tHEN, eLSE])) =
   mkBranch loc cond tHEN $ Just eLSE
-- Conversion
-- Elements
compileProg loc (In (App Ut.EMaterialize _ [len, arr])) = do
   len' <- mkLength len (Ut.typeof len)
   tellProg [initArray loc len']
   compileProg loc arr
compileProg (Just loc) (In (App Ut.EWrite _ [ix, e])) = do
   dst <- compileExpr ix
   compileProg (Just $ mkArrayElem loc [dst]) e
compileProg _ (In (App Ut.ESkip _ _)) = return ()
compileProg loc (In (App Ut.EPar _ [p1, p2])) = do
   (_, Block ds1 b1) <- confiscateBlock $ compileProg loc p1
   (_, Block ds2 b2) <- confiscateBlock $ compileProg loc p2
   tellProg [toProg $ Block (ds1 ++ ds2) (Sequence [b1,b2])]
compileProg (Just loc) (In (App Ut.EparFor _ [len, In (Ut.Lambda (Ut.Var v ta _) ixf)])) = do
   opts <- asks backendOpts
   let ix = mkVar (compileType opts ta) v
   len' <- mkLength len ta
   (ptyp, b) <- case ixf of
          In (App (Ut.Call Loop n) _ vs) -> do
            vs' <- mapM compileExpr vs
            let args  = map (ValueParameter . varToExpr) $ nub $ map varExpr vs' ++ fv loc
            return (TaskParallel, toBlock $ ProcedureCall n args)
          _                              -> do
            b' <- confiscateBlock $ compileProg (Just loc) ixf
            return (Parallel, snd b')
   tellProg [for ptyp (varExpr ix) (litI32 0) len' (litI32 1) b]
-- Error
compileProg _ (In (App Ut.Undefined _ _)) = return ()
compileProg loc (In (App (Ut.Assert msg) _ [cond, a])) = do
   compileAssert cond msg
   compileProg loc a
-- Future
compileProg _ e@(In (App Ut.MkFuture _ _))
  = error ("Unexpected MkFuture:" ++ show e)
compileProg (Just loc) (In (LetFun f e)) = do
   compileFunction loc f
   compileProg (Just loc) e
compileProg loc (In (App Ut.Await _ [a])) = do
   env <- ask
   fut <- compileExprVar a
   tellProg [iVarGet (inTask env) l fut | Just l <- [loc]]
-- Literal
compileProg loc (In (Ut.Literal a)) =
   case loc of
     Just l -> literalLoc l a
     Nothing -> return ()
-- Logic
-- Loop
compileProg (Just loc) (In (App Ut.ForLoop _ [len, init', In (Ut.Lambda (Ut.Var ix ta _) (In (Ut.Lambda (Ut.Var st _ _) ixf)))]))
  = do
      opts <- asks backendOpts
      let ix' = mkVar (compileType opts ta) ix
      len' <- mkLength len ta
      (lstate, stvar) <- mkDoubleBufferState loc st
      compileProg (Just lstate) init'
      (_, Block ds body) <- withAlias st lstate $ confiscateBlock
                          $ compileProg (Just stvar) ixf
                          >> shallowCopyWithRefSwap lstate stvar
      tellProg [toProg $ Block ds (for Sequential (varExpr ix') (litI32 0) len' (litI32 1) (toBlock body))]
      shallowAssign (Just loc) lstate
compileProg (Just loc) (In (App Ut.WhileLoop _ [init', In (Ut.Lambda (Ut.Var cv _ _) cond), In (Ut.Lambda (Ut.Var bv _ _) body)])) = do
    opts <- asks backendOpts
    let condv  = mkVariable (compileType opts (typeof cond)) cv
        condvE = varToExpr condv
    (lstate,stvar) <- mkDoubleBufferState loc bv
    compileProg (Just lstate) init'
    (_, cond') <- confiscateBlock $ withAlias cv lstate $ compileProg (Just condvE) cond
    (_, body') <- withAlias bv lstate $ confiscateBlock $ compileProg (Just stvar) body >> shallowCopyWithRefSwap lstate stvar
    declare condv
    tellProg [while cond' condvE body']
    shallowAssign (Just loc) lstate
-- LoopM
compileProg loc (In (App Ut.While _ [cond,step])) = do
   opts <- asks backendOpts
   condv <- freshVar opts "cond" (typeof cond)
   (_, cond') <- confiscateBlock $ compileProg (Just condv) cond
   (_, step') <- confiscateBlock $ compileProg loc step
   tellProg [while cond' condv step']
compileProg loc (In (App Ut.For _ [len, In (Ut.Lambda (Ut.Var v ta _) ixf)])) = do
   opts <- asks backendOpts
   let ix = mkVar (compileType opts ta) v
   len' <- mkLength len ta
   (_, Block ds body) <- confiscateBlock $ compileProg loc ixf
   tellProg [toProg $ Block ds (for Sequential (varExpr ix) (litI32 0) len' (litI32 1) (toBlock body))]
-- Mutable
compileProg loc (In (App Ut.Run _ [ma])) = compileProg loc ma
compileProg loc (In (App Ut.Return t [a]))
  | Ut.MutType (Ut.TupType []) <- t = return ()
  | Ut.ParType (Ut.TupType []) <- t = return ()
  | otherwise = compileProg loc a
compileProg loc (In (App Ut.Bind _ [ma, In (Ut.Lambda (Ut.Var v ta _) body)]))
  | (In (App Ut.ParNew _ _)) <- ma = do
   opts <- asks backendOpts
   let var = mkVariable (compileType opts ta) v
   declare var
   tellProg [iVarInit $ AddrOf $ varToExpr var]
   compileProg loc body
  | otherwise = do
   opts <- asks backendOpts
   let var = mkVariable (compileType opts ta) v
   declare var
   compileProg (Just (varToExpr var)) ma
   compileProg loc body
compileProg loc (In (App Ut.Then _ [ma, mb])) = do
   compileProg Nothing ma
   compileProg loc mb
compileProg loc (In (App Ut.When _ [c, action])) =
   mkBranch loc c action Nothing
-- MutableArray
compileProg loc (In (App Ut.NewArr _ [len, a])) = do
   nId <- freshId
   let var = mkNamedVar "i" (1 :# NumType Ut.Unsigned Ut.S32) nId
       ix  = varToExpr var
   a' <- compileExpr a
   l  <- compileExpr len
   tellProg [initArray loc l]
   tellProg [for Sequential var (litI32 0) l (litI32 1) $ toBlock (Sequence [copyProg (mkArrayElem <$> loc <*> pure [ix]) [a']])]
compileProg loc (In (App Ut.NewArr_ _ [len])) = do
   l <- compileExpr len
   tellProg [initArray loc l]
compileProg loc (In (App Ut.GetArr _ [arr, i])) = do
   arr' <- compileExpr arr
   i'   <- compileExpr i
   assign loc (mkArrayElem arr' [i'])
compileProg _ (In (App Ut.SetArr _ [arr, i, a])) = do
   arr' <- compileExpr arr
   i'   <- compileExpr i
   a'   <- compileExpr a
   assign (Just $ mkArrayElem arr' [i']) a'
-- MutableReference
compileProg loc (In (App Ut.NewRef _ [a])) = compileProg loc a
compileProg loc (In (App Ut.GetRef _ [r])) = compileProg loc r
compileProg _ (In (App Ut.SetRef _ [r, a])) = do
   var  <- compileExpr r
   compileProg (Just var) a
compileProg _ (In (App Ut.ModRef _ [r, In (Ut.Lambda (Ut.Var v _ _) body)])) = do
   var <- compileExpr r
   withAlias v var $ compileProg (Just var) body
       -- Since the modifier function is pure it is safe to alias
       -- v with var here
-- MutableToPure
compileProg (Just loc) (In (App Ut.RunMutableArray _ [marr]))
 | (In (App Ut.Bind _ [In (App Ut.NewArr_ _ [l]), In (Ut.Lambda (Ut.Var v _ _) body)])) <- marr
 , (In (App Ut.Return _ [In (Ut.Variable (Ut.Var r _ _))])) <- chaseBind body
 , v == r
 = do
     len <- compileExpr l
     tellProg [initArray (Just loc) len]
     withAlias v loc $ compileProg (Just loc) body
compileProg loc (In (App Ut.RunMutableArray _ [marr])) = compileProg loc marr
compileProg loc (In (App Ut.WithArray _ [marr@(In Ut.Variable{}), In (Ut.Lambda (Ut.Var v _ _) body)])) = do
    e <- compileExpr marr
    withAlias v e $ do
      b <- compileExpr body
      tellProg [copyProg loc [b]]
compileProg loc (In (App Ut.WithArray _ [marr, In (Ut.Lambda (Ut.Var v ta _) body)])) = do
    opts <- asks backendOpts
    let var = mkVariable (compileType opts ta) v
    declare var
    compileProg (Just $ varToExpr var) marr
    e <- compileExpr body
    tellProg [copyProg loc [e]]
-- Noinline
compileProg (Just _) (In (App Ut.NoInline _ [e]))
  = error ("Unexpected NoInline:" ++ show e)
-- Par
compileProg loc (In (App Ut.ParRun _ [p])) = compileProg loc p
compileProg _ (In (App Ut.ParNew _ _)) = return ()
compileProg loc (In (App Ut.ParGet _ [r])) = do
    env <- ask
    iv <- compileExpr r
    tellProg [iVarGet (inTask env) l iv | Just l <- [loc]]
compileProg _ (In (App Ut.ParPut _ [r, a])) = do
    iv  <- compileExpr r
    val <- compileExpr a
    i   <- freshId
    let var  = mkNamedVar "msg" (typeof val) i
        varE = varToExpr var
    declare var
    assign (Just varE) val
    tellProg [iVarPut iv varE]
compileProg _ (In (App Ut.ParFork _ [e]))
  = error ("Unexpected ParFork:" ++ show e)
compileProg _ (In (App Ut.ParYield _ _)) = return ()
-- SizeProp
compileProg loc (In (App Ut.PropSize _ [e])) = compileProg loc e
-- SourceInfo
compileProg loc (In (App (Ut.SourceInfo info) _ [a])) = do
    tellProg [Comment True info]
    compileProg loc a
-- Switch
compileProg loc (In (App Ut.Switch _ [tree@(In (App Ut.Condition _ [In (App Ut.Equal _ [_, s]), _, _]))])) = do
    scrutinee <- compileExpr s
    alts      <- chaseTree loc s tree
    tellProg [Switch{..}]
compileProg loc (In (App Ut.Switch _ [tree])) = compileProg loc tree
-- Tuple
compileProg loc (In (App Ut.Tup _ ms)) = sequence_
    [ compileProg (StructField <$> loc <*> pure ("member" ++ show n)) m
      | (n,m) <- zip [1 :: Int ..] ms
    ]
-- Special case foreign imports since they can be of void type and just have effects.
compileProg loc@(Just _) (In (App p@Ut.ForeignImport{} t es)) = do
    opts <- asks backendOpts
    es' <- mapM compileExpr es
    shallowAssign loc $ fun (compileType opts t) (compileOp p) es'
compileProg Nothing (In (App p@Ut.ForeignImport{} _ es)) = do
    es' <- mapM compileExpr es
    tellProg [ProcedureCall (compileOp p) $ map ValueParameter es']
-- Common nodes
compileProg (Just loc) (In (App (Ut.Call f name) _ es)) = do
  es' <- mapM compileExpr es
  let args = nub $ map exprToVar es' ++ fv loc
  tellProg [iVarInitCond f (AddrOf loc)]
  tellProg [spawn f name args]
compileProg loc e = compileExprLoc loc e

--------------------------------------------------------------------------------
-- * compileExpr
--------------------------------------------------------------------------------

-- 'compileExpr' should handle constructs whose can be represented as a
-- non-variable 'Expression' (e.g. a conditional), and delegate all other
-- constructs to 'compileProg'. Delegation is done by calling
-- 'compileProgFresh'.

-- | Compile an expression
compileExpr :: Ut.UntypedFeld -> CodeWriter (Expression ())
-- Array
compileExpr (In (App Ut.GetLength _ [a])) = do
   aExpr <- compileExpr a
   return $ arrayLength aExpr
compileExpr (In (App Ut.GetIx _ [arr, i])) = do
   a' <- compileExpr arr
   i' <- compileExpr i
   return $ mkArrayElem a' [i']
-- Bits
compileExpr (In (App Ut.Bit t [arr])) = do
   opts <- asks backendOpts
   a' <- compileExpr arr
   let t' = compileType opts t
   return $ binop t' "<<" (litI t' 1) a'
-- Binding
compileExpr (In (Ut.Variable (Ut.Var v t _))) = do
        env <- ask
        case lookup v (aliases env) of
          Nothing -> return $ mkVar (compileType (backendOpts env) t) v
          Just e  -> return e
compileExpr (In (Ut.App Ut.Let _ [a, In (Ut.Lambda (Ut.Var v ta _) body)])) = do
    e <- compileLet a ta v
    withAlias v e $ compileExpr body
-- Bits
-- Condition
-- Conversion
compileExpr (In (App Ut.F2I t es)) = do
    opts <- asks backendOpts
    es' <- mapM compileExpr es
    let f' = fun (1 :# FloatType) "truncf" es'
    return $ Cast (compileType opts t) f'
compileExpr (In (App Ut.I2N t1 [e])) = do
    opts <- asks backendOpts
    let t' = compileType opts t1
    case t' of
      1 :# (ComplexType t) -> do
        e' <- compileExpr e
        let args = [Cast t e', litF 0]
        return $ fun t' (extend c99 "complex" t) args
      _ -> do
        e' <- compileExpr e
        return $ Cast t' e'
compileExpr (In (App Ut.B2I t [e])) = do
    opts <- asks backendOpts
    e' <- compileExpr e
    return $ Cast (compileType opts t) e'
compileExpr (In (App Ut.Round t es)) = do
    opts <- asks backendOpts
    es' <- mapM compileExpr es
    let f' = fun (1 :# FloatType) "roundf" es'
    return $ Cast (compileType opts t) f'
compileExpr (In (App Ut.Ceiling t es)) = do
    opts <- asks backendOpts
    es' <- mapM compileExpr es
    let f' = fun (1 :# FloatType) "ceilf" es'
    return $ Cast (compileType opts t) f'
compileExpr (In (App Ut.Floor t es)) = do
    opts <- asks backendOpts
    es' <- mapM compileExpr es
    let f' = fun (1 :# FloatType) "floorf" es'
    return $ Cast (compileType opts t) f'
-- Error
compileExpr (In (App (Ut.Assert msg) _ [cond, a])) = do
    compileAssert cond msg
    compileExpr a
-- Eq
-- FFI
-- Floating
compileExpr (In (App Ut.Pi _ [])) = error "No pi ready"
-- Fractional
-- Future
-- Literal
compileExpr (In (Ut.Literal l)) = literal l
-- Loop
-- Logic
-- Mutable
compileExpr (In (App Ut.Run _ [ma])) = compileExpr ma
-- MutableArray
compileExpr (In (App Ut.ArrLength _ [arr])) = do
    a' <- compileExpr arr
    return $ arrayLength a'
-- MutableReference
compileExpr (In (App Ut.GetRef _ [r])) = compileExpr r
-- NoInline
-- Num
-- Ord
-- SizeProp
compileExpr (In (App Ut.PropSize _ [e])) = compileExpr e
-- SourceInfo
compileExpr (In (App (Ut.SourceInfo info) _ [a])) = do
    tellProg [Comment True info]
    compileExpr a
-- Tuple
compileExpr (In (App (Ut.Sel n) _ [tup])) = do
    tupExpr <- compileExpr tup
    return $ StructField tupExpr ("member" ++ show (n + 1))
compileExpr e@(In (App p _ _))
 | p `elem` [ Ut.Parallel, Ut.SetLength, Ut.Sequential, Ut.Condition, Ut.ConditionM
            , Ut.MkFuture, Ut.Await, Ut.Bind, Ut.Then, Ut.Return, Ut.While, Ut.For, Ut.SetArr, Ut.EMaterialize
            , Ut.WhileLoop, Ut.ForLoop, Ut.RunMutableArray, Ut.NoInline
            , Ut.Switch, Ut.WithArray, Ut.Tup]
 = compileProgFresh e
compileExpr (In (App p t es)) = do
    opts <- asks backendOpts
    es' <- mapM compileExpr es
    return $ fun (compileType opts t) (compileOp p) es'
compileExpr e = compileProgFresh e



--------------------------------------------------------------------------------
-- * Compilation helper functions
--------------------------------------------------------------------------------

{-

Precise size information
------------------------

Tight size bounds for a given literal is easy to compute. Precise
bounds are particularly important for array literals since they are
often copied in the deepCopy function near the CodeGen in the
backend. Deepcopy will appear to hang when generating the copy code
for insanely large array literals, so don't do that.

-}

-- | Call 'compileExpr' and assign the result to the given location.
compileExprLoc :: Location  -> Ut.UntypedFeld  -> CodeWriter ()
compileExprLoc loc e = do
    expr <- compileExpr e
    assign loc expr

-- | Generate and declare a fresh variable expression
freshVar :: Options -> String -> Ut.Type -> CodeWriter (Expression ())
freshVar opt base t = do
  v <- mkNamedVar base (compileType opt t) <$> freshId
  declare v
  return $ varToExpr v

-- | Compiles code into a fresh variable.
compileProgFresh :: Ut.UntypedFeld -> CodeWriter (Expression ())
compileProgFresh e = do
    opts <- asks backendOpts
    loc <- freshVar opts "e" (typeof e)
    compileProg (Just loc) e
    return loc

-- | Compile an expression and make sure that the result is stored in a variable
compileExprVar :: Ut.UntypedFeld -> CodeWriter (Expression ())
compileExprVar e = do
    e' <- compileExpr e
    case e' of
        _ | isNearlyVar e' -> return e'
          | otherwise -> do
              varId <- freshId
              let loc  = mkNamedVar "e" (typeof e') varId
                  locE = varToExpr loc
              declare loc
              assign (Just locE) e'
              return locE
  where isNearlyVar VarExpr{}   = True
        isNearlyVar (Deref e')  = isNearlyVar e'
        isNearlyVar (AddrOf e') = isNearlyVar e'
        isNearlyVar _           = False

-- | Compile a function bound by a LetFun.
compileFunction :: Expression () -> (String, Fork, Ut.UntypedFeld) -> CodeWriter ()
compileFunction loc (coreName, kind, e) | (bs, e') <- collectBinders e = do
  es' <- mapM (compileExpr . In . Ut.Variable) bs
  let args = nub $ map exprToVar es' ++ fv loc
  -- Task core:
  (_, (Block ds bl, decls, _)) <- confiscateBigBlock $
    case kind of
      Future -> do
        p' <- local (\env -> env {inTask = True }) $ compileExprVar e'
        tellProg [iVarPut loc p']
      Par -> local (\env -> env {inTask = True }) $ compileProg (Just loc) e'
      Loop | (_:_) <- es'
           , Ut.ElementsType{} <- typeof e' -> compileProg (Just loc) e'
           | (ix:_) <- es' -> compileProg (Just $ mkArrayElem loc [ix]) e'
      None -> compileProg (Just loc) e'
  tellDef [Proc coreName (kind == Loop) args VoidType $ Just $ Block (decls ++ ds) bl]
  -- Task:
  let taskName = "task" ++ drop 9 coreName
      runTask  = Just $ toBlock $ run coreName args
      formals  = [mkNamedRef "params" VoidType (-1)]
  case kind of
   _ | kind `elem` [None, Loop] -> return ()
   _    -> tellDef [Proc taskName False formals VoidType runTask]

-- | Check if an expression is a variable or a literal
isVariableOrLiteral :: Ut.UntypedFeld -> Bool
isVariableOrLiteral (Ut.In Ut.Literal{})  = True
isVariableOrLiteral (Ut.In Ut.Variable{}) = True
isVariableOrLiteral _                     = False

-- | Create a variable of the right type for storing a length.
mkLength :: Ut.UntypedFeld -> Ut.Type -> CodeWriter (Expression ())
mkLength a t
  | isVariableOrLiteral a = compileExpr a
  | otherwise             = do
      opts <- asks backendOpts
      lenvar <- freshVar opts "len" t
      compileProg (Just lenvar) a
      return lenvar

mkBranch :: Location -> Ut.UntypedFeld -> Ut.UntypedFeld -> Maybe Ut.UntypedFeld -> CodeWriter ()
mkBranch loc c th el = do
    ce <- compileExpr c
    (_, tb) <- confiscateBlock $ compileProg loc th
    (_, eb) <- if isJust el
                  then confiscateBlock $ compileProg loc (fromJust el)
                  else return (undefined, toBlock Empty)
    tellProg [Switch ce [(Pat (litB True), tb), (Pat (litB False), eb)]]

compileLet :: Ut.UntypedFeld -> Ut.Type -> VarId -> CodeWriter (Expression ())
compileLet a ta v = do
   opts <- asks backendOpts
   let var  = mkVariable (compileType opts ta) v
       varE = varToExpr var
   declare var
   compileProg (Just varE) a
   return varE

compileAssert :: Ut.UntypedFeld -> String -> CodeWriter ()
compileAssert cond msg = do
    condExpr <- compileExpr cond
    tellProg [call "assert" [ValueParameter condExpr]]
    unless (null msg) $ tellProg [Comment False $ "{" ++ msg ++ "}"]

literal :: Ut.Lit -> CodeWriter (Expression ())
literal lit = do
    opts <- asks backendOpts
    let litConst = return $ ConstExpr $ literalConst opts lit
    case lit of
      LTup []     -> litConst
      LBool{}     -> litConst
      LInt{}      -> litConst
      LFloat{}    -> litConst
      LDouble{}   -> litConst
      LComplex{}  -> litConst
      LArray{}    -> litConst
      _           -> do loc <- freshVar opts "x" (typeof lit)
                        literalLoc loc lit
                        return loc

-- | Returns true if we can represent the literal in Program.
representableType :: Ut.Lit -> Bool
representableType l
  | Ut.ArrayType{} <- t = True
  -- Simple types.
  | _ Ut.:# Ut.IntType{} <- t     = True
  | _ Ut.:# Ut.ComplexType{} <- t = True
  | _ Ut.:# st <- t
  = st `elem` [Ut.DoubleType, Ut.FloatType, Ut.BoolType]
  | otherwise
  = t == Ut.TupType []
      where t = typeof l

literalConst :: Options -> Ut.Lit -> Constant ()
literalConst _   (LTup [])      = IntConst 0 (NumType Ut.Unsigned Ut.S32)
literalConst _   (LBool a)      = BoolConst a
literalConst _   (LInt s sz a)  = IntConst (toInteger a) (NumType s sz)
literalConst _   (LFloat a)     = FloatConst a
literalConst _   (LDouble a)    = DoubleConst a
literalConst opt (LArray t es)  = ArrayConst (map (literalConst opt) es) $ compileType opt t
literalConst opt (LComplex r i) = ComplexConst (literalConst opt r) (literalConst opt i)

literalLoc :: Expression () -> Ut.Lit -> CodeWriter ()
literalLoc loc arr@Ut.LArray{} = do
    opts <- asks backendOpts
    tellProg [copyProg (Just loc) [ConstExpr $ literalConst opts arr]]
literalLoc loc (Ut.LTup ls) = sequence_
    [literalLoc (StructField loc ("member" ++ show n)) l | (n,l) <- zip [1 :: Int ..] ls]
literalLoc loc t =
    do rhs <- literal t
       assign (Just loc) rhs

chaseTree :: Location -> Ut.UntypedFeld -> Ut.UntypedFeld -> CodeWriter [(Pattern (), Block ())]
chaseTree loc _s (In (App Ut.Condition _ [In (App Ut.Equal _ [c, _]), t, f]))
    -- , alphaEq s a -- TODO check that the scrutinees are equal
    = do
         e <- compileExpr c
         (_,body) <- confiscateBlock $ compileProg loc t
         cases <- chaseTree loc _s f
         return $ (Pat e, body) : cases

chaseTree loc _ a = do
    (_,body) <- confiscateBlock $ compileProg loc a
    return [(PatDefault, body)]

-- | Chase down the right-spine of `Bind` and `Then` constructs and return
-- the last term
chaseBind :: Ut.UntypedFeld -> Ut.UntypedFeld
chaseBind (In (App Ut.Let  _ [_, In (Ut.Lambda _  body)])) = chaseBind body
chaseBind (In (App Ut.Bind _ [_, In (Ut.Lambda _  body)])) = chaseBind body
chaseBind (In (App Ut.Then _ [_, body]))                   = chaseBind body
chaseBind a                                                = a

{- NOTES:

The trick of doing a copy at the end, i.e. `tellProg [copyProg loc
e]`, when compiling WithArray is important. It allows us to safely
return the pure array that is passed in as input. This is nice because
it allows us to implement `freezeArray` in terms of `withArray`.
In most cases I expect `withArray` to return a scalar as its final
result and then the copyProg is harmless.
-}

compileBind :: (Ut.Var, Ut.UntypedFeld) -> CodeWriter ()
compileBind (Ut.Var v t _, e) = do
   opts <- asks backendOpts
   let var = mkVariable (compileType opts t) v
   declare var
   compileProg (Just $ varToExpr var) e

-- | Translates Op names to strings.
compileOp :: Ut.Op -> String
-- Bits
compileOp Ut.BAnd              = "&"
compileOp Ut.BOr               = "|"
compileOp Ut.BXor              = "^"
-- Complex
compileOp Ut.RealPart          = "creal"
compileOp Ut.ImagPart          = "cimag"
compileOp Ut.Sign              = "signum"
  -- Eq
compileOp Ut.Equal             = "=="
compileOp Ut.NotEqual          = "/="
  -- FFI
compileOp (Ut.ForeignImport s) = s
  -- Floating
compileOp Ut.Exp               = "exp"
  -- Fractional
compileOp Ut.DivFrac           = "/"
  -- Integral
compileOp Ut.IExp              = "pow"
  -- Logic
compileOp Ut.And               = "&&"
compileOp Ut.Or                = "||"
  -- Num
compileOp Ut.Add               = "+"
compileOp Ut.Sub               = "-"
compileOp Ut.Mul               = "*"
  -- Ord
compileOp Ut.LTH               = "<"
compileOp Ut.GTH               = ">"
compileOp Ut.LTE               = "<="
compileOp Ut.GTE               = ">="
compileOp p                    = toLower h:t
    where (h:t) = show p

-- FIXME: Remove this definition of collectLetBinders.
collectLetBinders :: UntypedFeld -> ([(Ut.Var, UntypedFeld)], UntypedFeld)
collectLetBinders = go []
  where go acc (In (App Ut.Let _ [e, In (Ut.Lambda v b)])) = go ((v, e):acc) b
        go acc e                                           = (reverse acc, e)

-- FIXME: Remove this definition of collectLetBinders.
collectBinders :: UntypedFeld -> ([Ut.Var], UntypedFeld)
collectBinders = go []
  where go acc (In (Ut.Lambda v e)) = go (v:acc) e
        go acc e                    = (reverse acc, e)
