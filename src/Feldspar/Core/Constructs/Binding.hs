{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

{-# LANGUAGE UndecidableInstances #-}

-- | Interpretation of binding constructs

module Feldspar.Core.Constructs.Binding
    ( module Language.Syntactic.Constructs.Binding
    , optimizeLambda
    , optimizeFunction
--    , optimizeFunctionFix
    , prjLambda
    , betaReduce
    ) where

import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Data.Typeable (Typeable, gcast)

import Data.Lens.Common
import Data.Proxy

import Language.Syntactic
import Language.Syntactic.Constructs.Binding hiding (subst,betaReduce)
import Language.Syntactic.Constructs.Binding.HigherOrder (ArgConstr(..))

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation

instance Sharable Variable
  -- Will not be shared anyway, because it's a terminal

instance Sharable Lambda
  where
    sharable _ = False

instance Sharable Let

-- | Should be a capture-avoiding substitution, but it is currently not correct.
--
-- Note: Variables with a different type than the new expression will be
-- silently ignored.
subst :: forall constr dom a b
    .  ( Constrained dom
       , ArgConstr Lambda Type :<: dom
       , (Variable :|| Type) :<: dom
       )
    => VarId       -- ^ Variable to be substituted
    -> ASTF (dom :|| Typeable) a  -- ^ Expression to substitute for
    -> ASTF (dom :|| Typeable) b  -- ^ Expression to substitute in
    -> ASTF (dom :|| Typeable) b
subst v new a = go a
  where
    go :: AST (dom :|| Typeable) c -> AST (dom :|| Typeable) c
    go a@((prjLambda -> Just (ArgConstr (Lambda w))) :$ _)
        | v==w = a  -- Capture
    go (f :$ a) = go f :$ go a
    go var
        | Just (C' (Variable w)) <- prjF var
        , v==w
        , Dict :: Dict (Typeable a) <- exprDictSub new
        , Dict :: Dict (Typeable x) <- exprDictSub var
        , Just new' <- gcast new
        = new'
    go a = a
  -- TODO Make it correct (may need to alpha-convert `new` before inserting it)
  -- TODO Should there be an error if `gcast` fails? (See note in Haddock
  --      comment.)

betaReduce
    :: ( Constrained dom
       , ArgConstr Lambda Type :<: dom
       , (Variable :|| Type) :<: dom
       )
    => ASTF (dom :|| Typeable) a         -- ^ Argument
    -> ASTF (dom :|| Typeable) (a -> b)  -- ^ Function to be reduced
    -> ASTF (dom :|| Typeable) b
betaReduce new (lam :$ body)
    | Just (ArgConstr (Lambda v)) <- prjLambda lam = subst v new body

optimizeLambda :: ( ArgConstr Lambda Type :<: dom
                  , OptimizeSuper dom)
    => (ASTF (dom :|| Typeable) b -> Opt (ASTF (Decor Info (dom :|| Typeable)) b))  -- ^ Optimization of the body
    -> Info a
    -> ArgConstr Lambda Type (b :-> Full (a -> b))
    -> Args (AST (dom :|| Typeable)) (b :-> Full (a -> b))
    -> Opt (ASTF (Decor Info (dom :|| Typeable)) (a -> b))
optimizeLambda opt info lam@(ArgConstr (Lambda v)) (body :* Nil)
    | Dict <- exprDict body
    = do
        body' <- localVar v info $ opt body
        constructFeatUnOpt lam (body' :* Nil)

-- | Assumes that the expression is a 'Lambda'
optimizeFunction :: ( ArgConstr Lambda Type :<: dom
                    , OptimizeSuper dom)
    => (ASTF (dom :|| Typeable) b -> Opt (ASTF (Decor Info (dom :|| Typeable)) b))  -- ^ Optimization of the body
    -> Info a
    -> (ASTF (dom :|| Typeable) (a -> b) -> Opt (ASTF (Decor Info (dom :|| Typeable)) (a -> b)))
optimizeFunction opt info a@(sym :$ body)
    | Dict <- exprDict a
    , Dict <- exprDict body
    , Just (lam@(ArgConstr (Lambda v))) <- prjLambda sym
    = optimizeLambda opt info lam (body :* Nil)

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
    constructFeatUnOpt var@(C' (Variable v)) Nil
        = reader $ \env -> case Prelude.lookup v (varEnv env) of
            Nothing -> error $
                "optimizeFeat: can't get size of free variable: v" ++ show v
            Just (SomeInfo info) ->
                let info' = (fromJust $ gcast info) {infoVars = singleton v (SomeType $ infoType info) }
                 in Sym $ Decor info' $ C' $ inj $ c' (Variable v)

instance ( ArgConstr Lambda Type :<: dom
         , OptimizeSuper dom)
      => Optimize (ArgConstr Lambda Type) dom
  where
    -- | Assigns a 'universal' size to the bound variable. This only makes sense
    -- for top-level lambdas. For other uses, use 'optimizeLambda' instead.

    optimizeFeat lam@(ArgConstr (Lambda v))
        | Dict <- exprDict lam
        = optimizeLambda optimizeM (mkInfo universal) lam

    constructFeatUnOpt lam@(ArgConstr (Lambda v)) (body :* Nil)
        | Dict <- exprDict lam
        , Info t sz vars _ <- getInfo body
        = do
            src <- asks sourceEnv
            let info = Info (FunType typeRep t) sz (delete v vars) src
            return $ (Sym $ Decor info $ C' $ inj lam) :$ body

instance SizeProp (Let :|| Type)
  where
    sizeProp (C' Let) (_ :* WrapFull f :* Nil) = infoSize f

instance
    ( (Let      :|| Type)   :<: dom
    , (Variable :|| Type)   :<: dom
    , ArgConstr Lambda Type :<: dom
    , OptimizeSuper dom
    ) =>
      Optimize (Let :|| Type) dom
  where
    optimizeFeat lt@(C' Let) (a :* f :* Nil) = do
        a' <- optimizeM a
        f' <- optimizeFunction optimizeM (getInfo a') f
        case getInfo f' of
          Info{} -> constructFeat lt (a' :* f' :* Nil)
            -- TODO Why is this pattern match needed?

    constructFeatOpt (C' Let) (a :* (lam :$ var) :* Nil)
        | Just (C' (Variable v2))      <- prjF var
        , Just (ArgConstr (Lambda v1)) <- prjLambda lam
        , v1 == v2
        = return $ fromJust $ gcast a

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt x@(C' _) = constructFeatUnOptDefault x

prjLambda :: (Project (ArgConstr Lambda Type) dom)
          => dom sig -> Maybe ((ArgConstr Lambda Type) sig)
prjLambda = prj

