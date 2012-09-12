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
    , optimizeFunctionFix
    ) where

import Control.Monad.Reader
import Data.Maybe
import Data.Map
import Data.Typeable (Typeable, gcast)

import Data.Lens.Common
import Data.Proxy

import Language.Syntactic
import Language.Syntactic.Constructs.Binding

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation



instance Sharable (Variable TypeCtx)
  -- Will not be shared anyway, because it's a terminal

instance Sharable (Lambda TypeCtx)
  where
    sharable _ = False

instance Sharable (Let TypeCtx TypeCtx)



optimizeLambda :: (Lambda TypeCtx :<: dom, Optimize dom dom)
    => (ASTF dom b -> Opt (ASTF (Decor Info dom) b))  -- ^ Optimization of the body
    -> Info a
    -> Lambda TypeCtx (b :-> Full (a -> b))
    -> Args (AST dom) (b :-> Full (a -> b))
    -> Opt (ASTF (Decor Info dom) (a -> b))
optimizeLambda opt info lam@(Lambda v) (body :* Nil) = do
    body' <- localVar v info $ opt body
    constructFeatUnOpt lam (body' :* Nil)

-- | Assumes that the expression is a 'Lambda'
optimizeFunction :: (Lambda TypeCtx :<: dom, Optimize dom dom)
    => (ASTF dom b -> Opt (ASTF (Decor Info dom) b))  -- ^ Optimization of the body
    -> Info a
    -> (ASTF dom (a -> b) -> Opt (ASTF (Decor Info dom) (a -> b)))
optimizeFunction opt info (lam :$ body)
    | Just (Lambda v) <- prjCtx typeCtx lam
    = optimizeLambda opt info (Lambda v) (body :* Nil)

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



instance (Variable TypeCtx :<: dom, Optimize dom dom) =>
    Optimize (Variable TypeCtx) dom
  where
    constructFeatUnOpt var@(Variable v) Nil
        | TypeWit <- fromSatWit $ witnessSat var
        = reader $ \env -> case Prelude.lookup v (varEnv env) of
            Nothing -> error $
                "optimizeFeat: can't get size of free variable: v" ++ show v
            Just (SomeInfo info) ->
                let info' = (fromJust $ gcast info) {infoVars = singleton v (SomeType $ infoType info) }
                in  injDecorCtx typeCtx info' (Variable v)

instance (Lambda TypeCtx :<: dom, Optimize dom dom) =>
    Optimize (Lambda TypeCtx) dom
  where
    -- | Assigns a 'universal' size to the bound variable. This only makes sense
    -- for top-level lambdas. For other uses, use 'optimizeLambda' instead.
    optimizeFeat lam@(Lambda v)
        | TypeWit <- witnessByProxy typeCtx (argProxy lam)
        = optimizeLambda optimizeM (mkInfo universal) (Lambda v)

    constructFeatUnOpt lam@(Lambda v) (body :* Nil)
        | TypeWit <- witnessByProxy typeCtx (argProxy lam)
        , Info t sz vars _ <- getInfo body
        = do
            src <- asks sourceEnv
            let info = Info (FunType typeRep t) sz (delete v vars) src
            return $ injDecorCtx typeCtx info (Lambda v) :$ body

argProxy :: Lambda TypeCtx (b :-> Full (a -> b)) -> Proxy a
argProxy (Lambda _) = Proxy

instance SizeProp (Let TypeCtx TypeCtx)
  where
    sizeProp Let (_ :* WrapFull f :* Nil) = infoSize f

instance
    ( Let TypeCtx TypeCtx :<: dom
    , Lambda TypeCtx :<: dom
    , Variable TypeCtx :<: dom
    , Optimize dom dom
    ) =>
      Optimize (Let TypeCtx TypeCtx) dom
  where
    optimizeFeat lt@Let (a :* f :* Nil) = do
        a' <- optimizeM a
        f' <- optimizeFunction optimizeM (getInfo a') f
        case getInfo f' of
          Info{} -> constructFeat lt (a' :* f' :* Nil)
            -- TODO Why is this pattern match needed?

    constructFeatOpt Let (a :* (lam :$ var) :* Nil)
        | Just (_,Lambda v1)   <- prjDecorCtx typeCtx lam
        , Just (_,Variable v2) <- prjDecorCtx typeCtx var
        , v1 == v2
        = return $ fromJust $ gcast a

    constructFeatOpt a args = constructFeatUnOpt a args

    constructFeatUnOpt = constructFeatUnOptDefault

