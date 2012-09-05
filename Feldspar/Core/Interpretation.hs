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
    , constructFeatUnOptDefaultTyp
    , constructFeatUnOptDefault
    , optimizeFeatDefault
    ) where



import Control.Monad.Reader
import Control.Monad.Writer
import Data.Map as Map
-- import Data.Set as Set
import Data.Typeable (Typeable)

import Language.Syntactic
import Language.Syntactic.Constructs.Decoration
import Language.Syntactic.Constructs.Literal
import Language.Syntactic.Constructs.Binding
import qualified Language.Syntactic.Constructs.Binding.Optimize as Synt  -- For Haddock

import Feldspar.Lattice
import Feldspar.Core.Types



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
sizePropDefault :: (WitnessSat feature, SatContext feature ~ TypeCtx) =>
    feature a -> Args (WrapFull Info) a -> Size (DenResult a)
sizePropDefault a _
    | TypeWit <- fromSatWit $ witnessSat a = universal



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
viewLiteral :: (Literal TypeCtx :<: dom) => ASTF (Decor info dom) a -> Maybe a
viewLiteral (prjDecorCtx typeCtx -> Just (_,Literal a)) = Just a
viewLiteral _ = Nothing

-- | Construct a 'Literal' decorated with 'Info'
literalDecorSrc :: (Type a, Literal TypeCtx :<: dom) =>
    SourceInfo -> a -> ASTF (Decor Info dom) a
literalDecorSrc src a = injDecor
    ((mkInfo (sizeOf a)) {infoSource = src})
    (Literal a `withContext` typeCtx)

-- | Construct a 'Literal' decorated with 'Info'
literalDecor :: (Type a, Literal TypeCtx :<: dom) =>
    a -> ASTF (Decor Info dom) a
literalDecor = literalDecorSrc ""
  -- Note: This function could get the 'SourceInfo' from the environment and
  -- insert it in the 'infoSource' field. But then it needs to be monadic which
  -- makes optimizations uglier.

-- | Replaces an expression with a literal if the type permits, otherwise
-- returns the expression unchanged.
constFold :: (MaybeWitnessSat TypeCtx dom, Literal TypeCtx :<: dom) =>
    SourceInfo -> ASTF (Decor Info dom) a -> a -> ASTF (Decor Info dom) a
constFold src expr a
    | Just TypeWit <- fromSatWit `fmap` maybeWitnessSat typeCtx expr
    = literalDecorSrc src a
constFold _ expr _ = expr

-- | Environment for optimization
type Opt = Reader Env



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
        :: (WitnessCons feature, OptimizeSuper dom)
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
    ( WitnessCons dom
    , MaybeWitnessSat TypeCtx dom
    , AlphaEq dom dom dom [(VarId, VarId)]
    , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
    , EvalBind dom
    , Literal TypeCtx :<: dom
    , Variable TypeCtx :<: dom
    , Lambda TypeCtx :<: dom
    , Optimize dom dom
    ) =>
      OptimizeSuper dom

instance
    ( WitnessCons dom
    , MaybeWitnessSat TypeCtx dom
    , AlphaEq dom dom dom [(VarId, VarId)]
    , AlphaEq dom dom (Decor Info dom) [(VarId, VarId)]
    , EvalBind dom
    , Literal TypeCtx :<: dom
    , Variable TypeCtx :<: dom
    , Lambda TypeCtx :<: dom
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
    , WitnessCons sub1
    , WitnessCons sub2
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
optimizeM :: OptimizeSuper dom => ASTF dom a -> Opt (ASTF (Decor Info dom) a)
optimizeM a = do
    aOpt <- transformNode optimizeFeat a
    let vars  = infoVars $ getInfo aOpt
        value = evalBind aOpt
        src   = infoSource $ getInfo aOpt
    if Map.null vars
       then return $ constFold src aOpt value
       else return aOpt
  -- TODO singleton range --> literal
  --      literal         --> singleton range

-- | Optimization of an expression. This function runs 'optimizeM' and extracts
-- the result.
optimize :: OptimizeSuper dom => ASTF dom a -> ASTF (Decor Info dom) a
optimize = flip runReader initEnv . optimizeM

-- | Convenient default implementation of 'constructFeatUnOpt'. Uses 'sizeProp'
-- to propagate size.
constructFeatUnOptDefaultTyp
    :: ( feature :<: dom
       , WitnessCons feature
       , SizeProp feature
       , Show (Size (DenResult a))
       )
    => TypeRep (DenResult a)
    -> feature a
    -> Args (AST (Decor Info dom)) a
    -> Opt (ASTF (Decor Info dom) (DenResult a))
constructFeatUnOptDefaultTyp typ feat args
    | ConsWit <- witnessCons feat
    = do
        src <- asks sourceEnv
        let sz   = sizeProp feat $ mapArgs (WrapFull . getInfo) args
            vars = Map.unions $ listArgs (infoVars . getInfo) args
        return $ appArgs (injDecor (Info typ sz vars src) feat) args

-- | Like 'constructFeatUnOptDefaultTyp' but without an explicit 'TypeRep'
constructFeatUnOptDefault
    :: ( feature :<: dom
       , WitnessCons feature
       , WitnessSat feature
       , SatContext feature ~ TypeCtx
       , SizeProp feature
       )
    => feature a
    -> Args (AST (Decor Info dom)) a
    -> Opt (ASTF (Decor Info dom) (DenResult a))
constructFeatUnOptDefault feat
    | ConsWit <- witnessCons feat
    , TypeWit <- fromSatWit $ witnessSat feat
    = constructFeatUnOptDefaultTyp typeRep feat

-- | Convenient default implementation of 'optimizeFeat'
optimizeFeatDefault
    :: ( WitnessCons feature
       , Optimize feature dom
       , OptimizeSuper dom
       )
    => feature a
    -> Args (AST dom) a
    -> Opt (ASTF (Decor Info dom) (DenResult a))
optimizeFeatDefault feat args
    | ConsWit <- witnessCons feat
    = constructFeat feat =<< mapArgsM optimizeM args

