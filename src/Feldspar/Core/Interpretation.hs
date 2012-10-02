{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

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

-- | Defines different interpretations of Feldspar programs

module Feldspar.Core.Interpretation
    ( module Language.Syntactic.Constructs.Decoration

    , targetSpecialization
    , Sharable (..)
    , sharableDecor
    , SizeProp (..)
    , sizePropDefault
    , resultType
    , SourceInfo
    , Info (..)
    , mkInfo
    , mkInfoTy
    , infoRange
    , LatticeSize1 (..)
    , viewLiteral
    , literalDecor
    , constFold
    , SomeInfo (..)
    , SomeType (..)
    , Env (..)
    , localVar
    , localSource
    , Opt
    , Optimize (..)
    , OptimizeSuper
    , constructFeat
    , optimizeM
    , optimize
--    , constructFeatUnOptDefaultTyp
    , constructFeatUnOptDefault
    , optimizeFeatDefault
    , injDecorC
    , prjF
    , c'
    ) where



import Control.Monad.Reader
import Data.Map as Map
import Data.Typeable (Typeable)

import Language.Syntactic
import Language.Syntactic.Constructs.Decoration
import Language.Syntactic.Constructs.Literal
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder

import Feldspar.Lattice
import Feldspar.Core.Types
import Feldspar.Core.Interpretation.Typed


--------------------------------------------------------------------------------
-- * Target specialization
--------------------------------------------------------------------------------

-- | Specialize the program for a target platform with the given native bit
-- width
targetSpecialization :: BitWidth n -> ASTF dom a -> ASTF dom a
-- TODO targetSpecialization :: BitWidth n -> ASTF dom a -> ASTF dom (TargetType n a)
targetSpecialization _ = id



--------------------------------------------------------------------------------
-- * Code motion
--------------------------------------------------------------------------------

class Sharable dom
  where
    sharable :: dom a -> Bool
    sharable _ = True

instance (Sharable sub1, Sharable sub2) => Sharable (sub1 :+: sub2)
  where
    sharable (InjL a) = sharable a
    sharable (InjR a) = sharable a

sharableDecor :: Sharable dom => Decor info dom a -> Bool
sharableDecor = sharable . decorExpr



--------------------------------------------------------------------------------
-- * Size propagation
--------------------------------------------------------------------------------

-- | Forwards size propagation
class SizeProp feature
  where
    -- | Size propagation for a symbol given a list of argument sizes
    sizeProp :: feature a -> Args (WrapFull Info) a -> Size (DenResult a)

-- | Convenient default implementation of 'sizeProp'
sizePropDefault :: (Type (DenResult a))
                => feature a -> Args (WrapFull Info) a -> Size (DenResult a)
sizePropDefault _ _ = universal

--------------------------------------------------------------------------------
-- * Optimization and type/size inference
--------------------------------------------------------------------------------

-- | Compute a type representation of a symbol's result type
resultType :: Type (DenResult a) => c a -> TypeRep (DenResult a)
resultType _ = typeRep

data SomeType
  where
    SomeType :: TypeRep a -> SomeType

type VarInfo = Map VarId SomeType

-- | Information about the source code of an expression
type SourceInfo = String

-- | Type and size information of a Feldspar program
data Info a
  where
    Info
      :: Show (Size a)
      => { infoType   :: TypeRep a
         , infoSize   :: Size a
         , infoVars   :: VarInfo
         , infoSource :: SourceInfo
         }
      -> Info a

instance Render Info
  where
    render i@(Info {}) = show (infoType i) ++ szStr ++ srcStr
      where
        szStr = case show (infoSize i) of
          "()" -> ""  -- TODO AnySize
          str  -> " | " ++ str

        srcStr = case infoSource i of
          ""  -> ""
          src -> " | " ++ src

instance Eq (Size a) => Eq (Info a)
  where
    ia == ib = infoSize ia == infoSize ib
      -- TODO

mkInfo :: Type a => Size a -> Info a
mkInfo sz = Info typeRep sz Map.empty ""

mkInfoTy :: (Show (Size a), Lattice (Size a)) => TypeRep a -> Info a
mkInfoTy t = Info t universal Map.empty ""

infoRange :: Type a => Info a -> RangeSet a
infoRange = sizeToRange . infoSize

-- | This class is used to allow constructs to be abstract in the monad. Its
-- purpose is similar to that of 'MonadType'.
class LatticeSize1 m
  where
    mergeSize :: Lattice (Size a) =>
        Info (m a) -> Size (m a) -> Size (m a) -> Size (m a)
  -- TODO Is this class needed? See comment to `MonadType`.

instance LatticeSize1 Mut
  where
    mergeSize _ = (\/)

-- | 'Info' with hidden result type
data SomeInfo
  where
    SomeInfo :: Typeable a => Info a -> SomeInfo

data Env = Env
    { varEnv    :: [(VarId, SomeInfo)]
    , sourceEnv :: SourceInfo
    }

-- | Initial environment
initEnv :: Env
initEnv = Env [] ""

-- | Insert a variable into the environment
localVar :: Typeable b => VarId -> Info b -> Opt a -> Opt a
localVar v info = local $ \env -> env {varEnv = (v, SomeInfo info):varEnv env}

-- | Change the 'SourceInfo' environment
localSource :: SourceInfo -> Opt a -> Opt a
localSource src = local $ \env -> env {sourceEnv = src}

-- | It the expression is a literal, its value is returned, otherwise 'Nothing'
viewLiteral :: forall info dom a. ((Literal :|| Type) :<: dom)
            => ASTF (Decor info (dom :|| Typeable)) a -> Maybe a
viewLiteral (prjF -> Just (C' (Literal a))) = Just a
viewLiteral _ = Nothing

tProxy :: PProxy Type
tProxy = PProxy

prjF :: Project (sub :|| Type) sup => sup sig -> Maybe ((sub :|| Type) sig)
prjF = prjC' tProxy

-- | Construct a 'Literal' decorated with 'Info'
literalDecorSrc :: (Type a, (Literal :|| Type) :<: dom) =>
    SourceInfo -> a -> ASTF (Decor Info (dom :|| Typeable)) a
literalDecorSrc src a = Sym $ Decor
    ((mkInfo (sizeOf a)) {infoSource = src})
    (C' $ inj $ c' $ Literal a)

c' :: (Type (DenResult sig)) => feature sig -> (feature :|| Type) sig
c' = C'

-- | Construct a 'Literal' decorated with 'Info'
literalDecor :: (Type a, (Literal :|| Type) :<: dom) =>
    a -> ASTF (Decor Info (dom :|| Typeable)) a
literalDecor = literalDecorSrc ""
  -- Note: This function could get the 'SourceInfo' from the environment and
  -- insert it in the 'infoSource' field. But then it needs to be monadic which
  -- makes optimizations uglier.

-- | Replaces an expression with a literal if the type permits, otherwise
-- returns the expression unchanged.
constFold :: (Typed dom, (Literal :|| Type) :<: dom) =>
    SourceInfo -> ASTF (Decor Info (dom :|| Typeable)) a -> a -> ASTF (Decor Info (dom :|| Typeable)) a
constFold src expr a
    | Just Dict <- typeDict expr
    = literalDecorSrc src a
constFold _ expr _ = expr

-- | Environment for optimization
type Opt = Reader Env

prjDecorC :: (sub :<: sup)
          => AST (Decor info (sup )) a -> Maybe (info (DenResult a), sub a)
prjDecorC a = do
    (Sym (Decor info b)) <- return a
    c                    <- prj b
    return (info, c)

injDecorC :: (InjectC sub sup (DenResult a))
          => info (DenResult a) -> sub a -> AST (Decor info sup) a
injDecorC info = Sym . Decor info . injC

-- | Basic optimization of a feature
--
-- This optimization is similar to 'Synt.Optimize', but it also performs size
-- inference. Size inference has to be done simultaneously with other
-- optimizations in order to avoid iterating the phases. (Size information may
-- help optimization and optimization may help size inference.)
class Optimize feature dom
  where
    -- | Top-down and bottom-up optimization of a feature
    optimizeFeat
        :: ( Typeable (DenResult a)
           , OptimizeSuper dom
           )
        => feature a
        -> Args (AST dom) a
        -> Opt (ASTF (Decor Info dom) (DenResult a))
    optimizeFeat = optimizeFeatDefault

    -- | Optimized construction of an expression from a symbol and its optimized
    -- arguments
    --
    -- Note: This function should normally not be called directly. Instead, use
    -- 'constructFeat' which has more accurate propagation of 'Info'.
    constructFeatOpt
        :: feature a
        -> Args (AST (Decor Info dom)) a
        -> Opt (ASTF (Decor Info dom) (DenResult a))
    constructFeatOpt = constructFeatUnOpt

    -- | Unoptimized construction of an expression from a symbol and its
    -- optimized arguments
    constructFeatUnOpt
        :: feature a
        -> Args (AST (Decor Info dom)) a
        -> Opt (ASTF (Decor Info dom) (DenResult a))


instance Optimize Empty dom
  where
    constructFeatUnOpt = error "Not implemented: constructFeatUnOpt for Empty"

-- These classes used to be super-classes of `Optimize`, but after switching to
-- GHC 7.4, that lead to looping dictionaries (at run time). The problem arises
-- when you make instances like
--
--     instance Optimize dom dom => Optimize MyConstruct dom
--
-- Since the second parameter does not change, this seems to create a loop
-- whenever you want to access super-class methods through a
-- `Optimize MyConstruct dom` constraint.
--
-- This may or may not be related to the following (unconfirmed) bug:
--
--   http://hackage.haskell.org/trac/ghc/ticket/5913
--
-- To revert the class hierarchy:
--
--   * Make `OptimizeSuper` (expanded) a super-class of `Optimize`
--   * Make `WitnessCons feature` a super-class of `Optimize`
--   * Replace the context of `optimizeFeat` with `Optimize dom dom`
--   * Replace all references to `OptimizeSuper dom` with `Optimize dom dom`
--   * Remove `OptimizeSuper`
class
    ( AlphaEq dom dom dom [(VarId, VarId)]
    , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
    , EvalBind dom
    , (Literal :|| Type) :<: dom
    , Optimize (Variable :|| Type) dom
    , Optimize (ArgConstr Lambda Type) dom
    , Typed dom
    , Constrained dom
    , Optimize dom dom
    ) =>
      OptimizeSuper dom

instance
    ( AlphaEq dom dom dom [(VarId, VarId)]
    , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
    , EvalBind dom
    , (Literal  :|| Type) :<: dom
    , Optimize (Variable :|| Type) dom
    , Optimize (ArgConstr Lambda Type) dom
    , Typed dom
    , Constrained dom
    , Optimize dom dom
    ) =>
      OptimizeSuper dom



-- TODO Optimization should throw an error when the size of a node is
--      over-constrained. It can only happen if there's a bug in the general
--      size inference, or if the user has stated invalid size constraints. In
--      both cases it may lead to incorrect optimizations, so throwing an error
--      seems preferable.

-- | Optimized construction of an expression from a symbol and its optimized
-- arguments
constructFeat :: Optimize feature dom
    => feature a
    -> Args (AST (Decor Info dom)) a
    -> Opt (ASTF (Decor Info dom) (DenResult a))
constructFeat a args = do
    aUnOpt <- constructFeatUnOpt a args
    aOpt   <- constructFeatOpt a args
    return $ updateDecor
        (\info -> info {infoSize = infoSize (getInfo aUnOpt)})
        aOpt
  -- This function uses `constructFeatOpt` for optimization and
  -- `constructFeatUnOpt` for size propagation. This is because
  -- `constructFeatOpt` may produce less accurate size information than
  -- `constructFeatUnOpt`.

  -- TODO It might be better to use `sizeProp` instead of `constructFeatUnOpt`
  --      (but this changes class dependencies a bit). Is there any other use of
  --      `constructFeatUnOpt`?

instance
    ( Optimize sub1 dom
    , Optimize sub2 dom
    ) =>
      Optimize (sub1 :+: sub2) dom
  where
    optimizeFeat (InjL a) = optimizeFeat a
    optimizeFeat (InjR a) = optimizeFeat a

    constructFeatOpt (InjL a) = constructFeatOpt a
    constructFeatOpt (InjR a) = constructFeatOpt a

    constructFeatUnOpt (InjL a) = constructFeatUnOpt a
    constructFeatUnOpt (InjR a) = constructFeatUnOpt a

-- | Optimization of an expression
--
-- In addition to running 'optimizeFeat', this function performs constant
-- folding on all closed expressions, provided that the type permits making a
-- literal.
optimizeM :: (OptimizeSuper dom)
          => ASTF dom a -> Opt (ASTF (Decor Info dom) a)
optimizeM a
    | Dict <- exprDict a
    = do
        aOpt <- matchTrans (\(C' x) -> optimizeFeat x) a
        let vars  = infoVars $ getInfo aOpt
            value = evalBind aOpt
            src   = infoSource $ getInfo aOpt
    --    return aOpt
        if Map.null vars
           then return $ constFold src aOpt value
           else return aOpt
      -- TODO singleton range --> literal
      --      literal         --> singleton range

-- | Optimization of an expression. This function runs 'optimizeM' and extracts
-- the result.
optimize :: ( Typeable a
            , OptimizeSuper dom
            )
         => ASTF dom a -> ASTF (Decor Info dom) a
optimize = flip runReader initEnv . optimizeM

-- | Convenient default implementation of 'constructFeatUnOpt'. Uses 'sizeProp'
-- to propagate size.
{-
constructFeatUnOptDefaultTyp
    :: ( feature :<: dom
       , Show (Size (DenResult a))
       )
    => TypeRep (DenResult a)
    -> feature a
    -> Args (AST (Decor Info (dom :|| Typeable))) a
    -> Opt (ASTF (Decor Info (dom :|| Typeable)) (DenResult a))
constructFeatUnOptDefaultTyp typ feat args
    = do
        src <- asks sourceEnv
        let sz   = undefined -- sizeProp feat $ mapArgs (WrapFull . getInfo) args
            vars = Map.unions $ listArgs (infoVars . getInfo) args
        return $ appArgs (Sym $ Decor (Info typ sz vars src) $ C' $ inj feat) args
-}

-- | Like 'constructFeatUnOptDefaultTyp' but without an explicit 'TypeRep'
constructFeatUnOptDefault
    :: ( feature :<: dom
       , SizeProp feature
       , Type (DenResult a)
       )
    => feature a
    -> Args (AST (Decor Info dom)) a
    -> Opt (ASTF (Decor Info dom) (DenResult a))
constructFeatUnOptDefault feat args
    = do
        src <- asks sourceEnv
        let sz   = sizeProp feat $ mapArgs (WrapFull . getInfo) args
            vars = Map.unions $ listArgs (infoVars . getInfo) args
        return $ appArgs (Sym $ Decor (Info typeRep sz vars src) $ C' $ inj feat) args

-- | Convenient default implementation of 'optimizeFeat'
optimizeFeatDefault
    :: ( Optimize feature dom
       , OptimizeSuper dom
       )
    => feature a
    -> Args (AST dom) a
    -> Opt (ASTF (Decor Info dom) (DenResult a))
optimizeFeatDefault feat args
    = constructFeat feat =<< mapArgsM optimizeM args

