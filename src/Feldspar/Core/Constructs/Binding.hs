{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
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

-- | Interpretation of binding constructs

module Feldspar.Core.Constructs.Binding
    ( module Language.Syntactic.Constructs.Binding
    , optimizeLambda
    , optimizeFunction
--    , optimizeFunctionFix
    , betaReduce
    , prjLambda
    , cLambda
    , reuseCLambda
    , collectLetBinders
    ) where

import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Data.Typeable (Typeable, gcast)

--import Data.Lens.Common

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (subst,betaReduce)
import Language.Syntactic.Constructs.Binding.HigherOrder (CLambda)

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

instance Sharable Variable  -- `codeMotion` will not share variables anyway
instance Sharable Lambda    -- Will not be shared anyway because we disallow variables of `->` type
instance Sharable Let

subst :: forall constr dom a b
    .  ( Constrained dom
       , CLambda Type :<: dom
       , (Variable :|| Type) :<: dom
       )
    => VarId                      -- ^ Variable to be substituted
    -> ASTF (dom :|| Typeable) a  -- ^ Expression to substitute for
    -> ASTF (dom :|| Typeable) b  -- ^ Expression to substitute in
    -> ASTF (dom :|| Typeable) b
subst v new a = go a
  where
    go :: AST (dom :|| Typeable) c -> AST (dom :|| Typeable) c
    go a@((prjLambda -> Just (SubConstr2 (Lambda w))) :$ _)
        | v==w = a  -- Capture
    go (f :$ a) = go f :$ go a
    go var
        | Just (C' (Variable w)) <- prjF var
        , v==w
        , Dict <- exprDictSub pTypeable new
        , Dict <- exprDictSub pTypeable var
        , Just new' <- gcast new
        = new'
    go a = a
  -- TODO Should be possible to use the one in Syntactic instead

betaReduce
    :: ( Constrained dom
       , CLambda Type :<: dom
       , (Variable :|| Type) :<: dom
       )
    => ASTF (dom :|| Typeable) a         -- ^ Argument
    -> ASTF (dom :|| Typeable) (a -> b)  -- ^ Function to be reduced
    -> ASTF (dom :|| Typeable) b
betaReduce new (lam :$ body)
    | Just (SubConstr2 (Lambda v)) <- prjLambda lam = subst v new body
  -- TODO Should be possible to use the one in Syntactic instead

optimizeLambda :: ( CLambda Type :<: dom
                  , OptimizeSuper dom)
    => FeldOpts
    -> (ASTF (dom :|| Typeable) b -> Opt (ASTF (Decor Info (dom :|| Typeable)) b))  -- ^ Optimization of the body
    -> Info a
    -> CLambda Type (b :-> Full (a -> b))
    -> Args (AST (dom :|| Typeable)) (b :-> Full (a -> b))
    -> Opt (ASTF (Decor Info (dom :|| Typeable)) (a -> b))
optimizeLambda opts opt info lam@(SubConstr2 (Lambda v)) (body :* Nil)
    | Dict <- exprDict body
    = do
        body' <- localVar v info $ opt body
        constructFeatUnOpt opts lam (body' :* Nil)

-- | Assumes that the expression is a 'Lambda'
optimizeFunction :: ( (Variable :|| Type) :<: dom
                    , CLambda Type :<: dom
                    , Let :<: dom
                    , OptimizeSuper dom
                    )
    => FeldOpts
    -> (ASTF (dom :|| Typeable) b -> Opt (ASTF (Decor Info (dom :|| Typeable)) b))  -- ^ Optimization of the body
    -> Info a
    -> (ASTF (dom :|| Typeable) (a -> b) -> Opt (ASTF (Decor Info (dom :|| Typeable)) (a -> b)))
optimizeFunction opts opt info e
    | e'@(bs, _) <- collectLetBinders e
    , not (Prelude.null bs)
    = optimizeLet opts opt info e'
optimizeFunction opts opt info a@(sym :$ body)
    | Dict <- exprDict a
    , Dict <- exprDict body
    , Just (lam@(SubConstr2 (Lambda v))) <- prjLambda sym
    = optimizeLambda opts opt info lam (body :* Nil)
optimizeFunction opts opt info a
    = error $ "optimizeFunction: AST is not a function: " ++ show a ++ "\n" ++ show (infoType info)

optimizeLet
    :: ( (Variable :|| Type) :<: dom
       , CLambda Type :<: dom
       , Let :<: dom
       , OptimizeSuper dom
       )
    => FeldOpts
    -> (ASTF (dom :|| Typeable) b -> Opt (ASTF (Decor Info (dom :|| Typeable)) b))  -- ^ Optimization of the body
    -> Info a
    -> ([(VarId, ASTB (dom :|| Typeable) Type)], ASTF (dom :|| Typeable) (a -> b))
    -> Opt (ASTF (Decor Info (dom :|| Typeable)) (a -> b))
optimizeLet opts opt info ((v, ASTB e):t, bd)
    | Dict <- exprDict bd
    , Dict <- exprDict e
    = do
      e'  <- optimizeM opts e
      bd' <- localVar v (getInfo e') $ optimizeLet opts opt info (t, bd)
      bd'' <- constructFeatUnOpt opts (cLambda v) (bd' :* Nil)
      constructFeatUnOpt opts Let (e' :* bd'' :* Nil)
optimizeLet opts opt info ([], e) = optimizeFunction opts opt info e

{-
optimizeFunBody :: (Lambda TypeCtx :<: dom, Optimize dom dom, Typeable a)
    => (ASTF dom a -> Opt (ASTF (Decor Info dom) a))  -- ^ Optimization of the body
    -> Env                                            -- ^ Environment (instead of using 'Opt')
    -> VarId                                          -- ^ Bound variable
    -> ASTF dom a                                     -- ^ Body
    -> Info a                                         -- ^ 'Info' of bound variable
    -> ASTF (Decor Info dom) a
optimizeFunBody opt env v body info =
    flip runReader env $ localVar v info $ opt body

-- | Assumes that the expression is a 'Lambda'
optimizeFunctionFix
    :: forall dom a
    .  (Lambda TypeCtx :<: dom, Optimize dom dom, Type a)
    => (ASTF dom a -> Opt (ASTF (Decor Info dom) a))  -- ^ Optimization of the body
    -> Info a
    -> (ASTF dom (a -> a) -> Opt (ASTF (Decor Info dom) (a -> a)))
optimizeFunctionFix opt info (lam :$ body)
    | Just (Lambda v) <- prjCtx typeCtx lam
    = do
        env <- ask

        let aLens :: Lens (Info a) (Size a)
            aLens = lens infoSize (\sz inf -> inf {infoSize = sz})

        let bLens :: Lens (ASTF (Decor Info dom) a) (Size a)
            bLens = lens (infoSize . getInfo)
                (\sz a -> updateDecor (\inf -> inf {infoSize = sz}) a)

        let body' = fst $ boundedLensedFixedPoint 1 aLens bLens
                (optimizeFunBody opt env v body)
                info
              -- Using 1 as bound is motivated by the fact that a higher number
              -- leads to exponential blowup when there are many nested
              -- iterations. Since it is probably uncommon to have very deeply
              -- nested loops, it might be fine to increase the bound. However
              -- it is not clear that we gain anything by doing so, other than
              -- in very special cases.

        constructFeatUnOpt (Lambda v `withContext` typeCtx) (body' :* Nil)
-}

instance ( (Variable :|| Type) :<: dom
         , OptimizeSuper dom)
      => Optimize (Variable :|| Type) dom
  where
    constructFeatUnOpt _ var@(C' (Variable v)) Nil
        = reader $ \env -> case Prelude.lookup v (varEnv env) of
            Nothing -> error $
                "optimizeFeat: can't get size of free variable: v" ++ show v
            Just (SomeInfo info) ->
                let info' = (fromJust $ gcast info) {infoVars = singleton v (SomeType $ infoType info) }
                 in Sym $ Decor info' $ C' $ inj $ c' (Variable v)

instance ( CLambda Type :<: dom
         , OptimizeSuper dom)
      => Optimize (CLambda Type) dom
  where
    -- | Assigns a 'universal' size to the bound variable. This only makes sense
    -- for top-level lambdas. For other uses, use 'optimizeLambda' instead.

    optimizeFeat opts lam@(SubConstr2 (Lambda v))
        | Dict <- exprDict lam
        = optimizeLambda opts (optimizeM opts) (mkInfo universal) lam

    constructFeatUnOpt _ lam@(SubConstr2 (Lambda v)) (body :* Nil)
        | Dict <- exprDict lam
        , Info t sz vars _ <- getInfo body
        = do
            src <- asks sourceEnv
            let info = Info (FunType typeRep t) (universal, sz) (delete v vars) src
            return $ (Sym $ Decor info $ C' $ inj lam) :$ body

instance SizeProp Let
  where
    sizeProp Let (_ :* WrapFull f :* Nil) = snd $ infoSize f

instance
    ( Let                 :<: dom
    , (Variable :|| Type) :<: dom
    , CLambda Type        :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize Let dom
  where
    optimizeFeat opts lt@Let (a :* f :* Nil) = do
        a' <- optimizeM opts a
        f' <- optimizeFunction opts (optimizeM opts) (getInfo a') f
        constructFeat opts lt (a' :* f' :* Nil)

    constructFeatOpt _ Let (a :* (lam :$ var) :* Nil)
        | Just (C' (Variable v2))       <- prjF var
        , Just (SubConstr2 (Lambda v1)) <- prjLambda lam
        , v1 == v2
        = return $ fromJust $ gcast a

    constructFeatOpt opts Let (var :* f :* Nil)
        | Just (C' (Variable v)) <- prjF var
        = optimizeM opts $ betaReduce (stripDecor var) (stripDecor f)

      -- (letBind (letBind e1 (\x -> e2)) (\y -> e3) ==>
      --           letBind e1 (\x -> letBind e2 (\y-> e3))
      --
      -- Test case:
      --
      -- stestL2 :: Data Index -> Data Length -> Data [[Index]]
      -- stestL2 m x = parallel x (\x1 -> let z = let y = x `mod` m in (y, y) in parallel 2 (\x -> fst z))
    constructFeatOpt opts lt1@Let ((lt2 :$ x :$ (lam :$ bd)) :* y :* Nil)
        | Just Let <- prj lt2
        , Just lam'@(SubConstr2 (Lambda v1)) <- prjLambda lam
        , SICS `inTarget` opts
        = do
             bb <- constructFeat opts lt1 (bd :* y :* Nil)
             bd' <- constructFeat opts (reuseCLambda lam') (bb :* Nil)
             constructFeatUnOpt opts Let (x :* bd' :* Nil)

    -- Hoist let-bound constants upwards.
    --
    -- (letBind e1 (\x -> letBind v (\y -> e2)) ==>
    --           letBind v (\y -> letBind e1 (\x-> e2))
    constructFeatOpt opts lt1@Let (e :* (lam1 :$ (lt2 :$ v :$ (lam2 :$ bd))) :* Nil)
        | Just Let <- prj lt2
        , Nothing <- viewLiteral e
        , Just _ <- viewLiteral v
        , Just lam1'@(SubConstr2 (Lambda{})) <- prjLambda lam1
        , Just lam2'@(SubConstr2 (Lambda{})) <- prjLambda lam2
        , SICS `inTarget` opts
        = do
             bb  <- constructFeat opts (reuseCLambda lam1') (bd :* Nil)
             bb' <- constructFeat opts lt1 (e :* bb :* Nil)
             bd' <- constructFeat opts (reuseCLambda lam2') (bb' :* Nil)
             constructFeatUnOpt opts Let (v :* bd' :* Nil)

    constructFeatOpt opts a args = constructFeatUnOpt opts a args

    constructFeatUnOpt opts Let args@(_ :* (lam :$ body) :* Nil)
        | Just (SubConstr2 (Lambda _)) <- prjLambda lam
        , Info {infoType = t} <- getInfo body
        = constructFeatUnOptDefaultTyp opts t Let args

prjLambda :: (Project (CLambda Type) dom)
          => dom sig -> Maybe (CLambda Type sig)
prjLambda = prj

cLambda :: Type a => VarId -> CLambda Type (b :-> Full (a -> b))
cLambda = SubConstr2 . Lambda

-- | Allow an existing binding to be used with a body of a different type
reuseCLambda :: CLambda Type (b :-> Full (a -> b)) -> CLambda Type (c :-> Full (a -> c))
reuseCLambda (SubConstr2 (Lambda v)) = SubConstr2 (Lambda v)

-- | Collects the immediate let bindings in a list and returns the first non-let expression
--
-- This function can be useful when let bindings get in the way of pattern matching on a
-- sub-expressions.
collectLetBinders :: forall dom a .
                   ( Project Let dom
                   , Project (CLambda Type) dom
                   , ConstrainedBy dom Typeable
                   ) => ASTF dom a ->
                   ( [(VarId, ASTB dom Type)]
                   , ASTF dom a
                   )
collectLetBinders e = go [] e
  where
    go
      :: [(VarId, ASTB dom Type)]
      -> ASTF dom a
      -> ( [(VarId, ASTB dom Type)]
         , ASTF dom a
         )
    go bs (lt :$ e :$ (lam :$ body))
      | Just (SubConstr2 (Lambda v)) <- prjLambda lam
      , Just Let <- prj lt
      , Dict <- exprDict e
      = go ((v, ASTB e):bs) body
    go bs e = (reverse bs, e)

