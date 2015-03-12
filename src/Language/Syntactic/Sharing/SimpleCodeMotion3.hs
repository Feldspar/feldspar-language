{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Simple code motion transformation performing common sub-expression elimination and variable
-- hoisting. Note that the implementation is very inefficient.
--
-- The code is based on an implementation by Gergely Dévai.

module Language.Syntactic.Sharing.SimpleCodeMotion3
    ( mkSubEnvDefault
    , codeMotion3
    , reifySmart
    ) where



import Control.Applicative (Const (..))
import Control.Monad.State
import Data.Set as Set hiding (size)
import Data.Typeable

import Language.Syntactic
import Language.Syntactic.Constructs.Binding
import Language.Syntactic.Constructs.Binding.HigherOrder
import Language.Syntactic.Sharing.SimpleCodeMotion (PrjDict (..), InjDict (..), MkInjDict, prjDictFO)



-- | Substituting a sub-expression. Assumes no variable capturing in the
-- expressions involved.
substitute :: forall dom a b
    .  (ConstrainedBy dom Typeable, AlphaEq dom dom dom [(VarId,VarId)])
    => ASTF dom a  -- ^ Sub-expression to be replaced
    -> ASTF dom a  -- ^ Replacing sub-expression
    -> ASTF dom b  -- ^ Whole expression
    -> ASTF dom b
substitute x y a
    | Dict <- exprDictSub pTypeable y
    , Dict <- exprDictSub pTypeable a
    , Just y' <- gcast y, alphaEq x a = y'
    | otherwise = subst a
  where
    subst :: AST dom c -> AST dom c
    subst (f :$ a) = subst f :$ substitute x y a
    subst a = a

-- | Count the number of occurrences of a sub-expression
count :: forall dom a b
    .  AlphaEq dom dom dom [(VarId,VarId)]
    => ASTF dom a  -- ^ Expression to count
    -> ASTF dom b  -- ^ Expression to count in
    -> Int
count a b
    | alphaEq a b = 1
    | otherwise   = cnt b
  where
    cnt :: AST dom c -> Int
    cnt (f :$ b) = cnt f + count a b
    cnt _        = 0

-- | Environment for the expression in the 'choose' function
data Env dom = Env
    { inLambda :: Bool  -- ^ Whether the current expression is inside a lambda
    , counter  :: ASTE dom -> Int
        -- ^ Counting the number of occurrences of an expression in the
        -- environment
    , dependencies :: Set VarId
        -- ^ The set of variables that are not allowed to occur in the chosen
        -- expression
    }

independent :: PrjDict dom -> Env dom -> AST dom a -> Bool
independent pd env (Sym (prjVariable pd -> Just v)) = not (v `member` dependencies env)
independent pd env (f :$ a) = independent pd env f && independent pd env a
independent _ _ _ = True

isVariable :: PrjDict dom -> ASTF dom a -> Bool
isVariable pd (Sym (prjVariable pd -> Just _)) = True
isVariable pd _ = False

-- | Checks whether a sub-expression in a given environment can be lifted out
liftable :: PrjDict dom -> Env dom -> ASTF dom a -> Bool
liftable pd env a = independent pd env a && not (isVariable pd a) && heuristic
    -- Lifting dependent expressions is semantically incorrect
    -- Lifting variables would cause `codeMotion` to loop
  where
    heuristic = inLambda env || (counter env (ASTE a) > 1)

-- | Update the environment when going under a lambda that binds the given variable. This lambda is
-- assumed to be called multiple times, so that hoisting is worthwhile.
lamEnv :: VarId -> Env dom -> Env dom
lamEnv v env = env
    { inLambda     = True
    , dependencies = insert v (dependencies env)
    }

-- | Update the environment when going under a lambda that binds the given variable. This lambda is
-- assumed to be called only once, so that hoisting is not worthwhile.
lamEnvOneShot :: VarId -> Env dom -> Env dom
lamEnvOneShot v env = env
    { inLambda     = False
    , dependencies = insert v (dependencies env)
    }

-- | Function that gives a list of sub-expressions and their environment. A 'Nothing' result means
-- \"default behavior\" (see 'mkSubEnvDefault').
--
-- The purpose of this function is to be able to declare certain binding constructs as "one-shot".
-- This is done by not setting the 'inLambda' flag for sub-expressions that are under one-shot
-- lambdas but rather inherit it from the parent environment.
data MkSubEnv dom = MkSubEnv
    { mkSubEnv :: forall sig .
        Env dom -> dom sig -> Args (AST dom) sig -> Maybe [(ASTE dom, Env dom)]
    }

-- | Default implementation of 'MkSubEnv'
mkSubEnvDefault :: MkSubEnv dom
mkSubEnvDefault = MkSubEnv $ \_ _ _ -> Nothing

-- | List the sub-expressions and their environments. 'Let' bindings are handled first, to make sure
-- that these are always treated as one-shot binders. Next, the supplied 'MkSubEnv' is given the
-- chance to compute the result. If it doesn't, the expression is handled in a standard way.
subTermsEnv :: Project Let dom
    => PrjDict dom
    -> MkSubEnv dom
    -> Env dom
    -> dom sig
    -> Args (AST dom) sig
    -> [(ASTE dom, Env dom)]
subTermsEnv pd ed env lt (a :* (Sym lam :$ b) :* Nil)
    | Just Let <- prj lt
    , Just v   <- prjLambda pd lam
    = [(ASTE a, env), (ASTE b, lamEnvOneShot v env)]
subTermsEnv pd ed env s args
    | Just as <- mkSubEnv ed env s args = as
subTermsEnv pd ed env lam (body :* Nil)
    | Just v <- prjLambda pd lam
    = [(ASTE body, lamEnv v env)]
subTermsEnv pd ed env _ args = listArgs (\a -> (ASTE a, env)) args



-- | A sub-expression chosen to be shared together with an evidence that it can actually be shared
-- in the whole expression under consideration
data Chosen dom a
  where
    Chosen :: InjDict dom b a -> ASTF dom b -> Chosen dom a

-- | Choose a sub-expression to share
choose :: forall dom a
    .  (AlphaEq dom dom dom [(VarId,VarId)], Project Let dom)
    => (forall c. ASTF dom c -> Bool)
    -> PrjDict dom
    -> MkInjDict dom
    -> MkSubEnv dom
    -> ASTF dom a
    -> Maybe (Chosen dom a)
choose hoistOver pd mkId mkSub a = chooseEnvSub initEnv a
  where
    initEnv = Env
        { inLambda     = False
        , counter      = \(ASTE b) -> count b a
        , dependencies = empty
        }

    chooseEnv :: Env dom -> ASTF dom b -> Maybe (Chosen dom a)
    chooseEnv env b = this `mplus` that  -- Prefer `this` because it must be larger than `that`
      where
        this = do
            guard $ liftable pd env b
            id <- mkId b a
            return $ Chosen id b
        that = do
            guard $ hoistOver b
            chooseEnvSub env b

    -- | Like 'chooseEnv', but does not consider the top expression for sharing
    chooseEnvSub :: Env dom -> ASTF dom b -> Maybe (Chosen dom a)
    chooseEnvSub env a
        = Prelude.foldr (\(ASTE b, e) a -> chooseEnv e b `mplus` a) Nothing
        $ simpleMatch (subTermsEnv pd mkSub env) a



-- | Perform common sub-expression elimination and variable hoisting
codeMotion :: forall dom m a
    .  ( ConstrainedBy dom Typeable
       , AlphaEq dom dom dom [(VarId,VarId)]
       , Project Let dom
       , MonadState VarId m
       )
    => (forall c. ASTF dom c -> Bool)
         -- ^ Control wether a sub-expression can be hoisted over the given expression
    -> PrjDict dom
    -> MkInjDict dom
    -> MkSubEnv dom
    -> ASTF dom a
    -> m (ASTF dom a)
codeMotion hoistOver pd mkId mkSub a
    | Just (Chosen id b) <- choose hoistOver pd mkId mkSub a = share id b
    | otherwise = descend a
  where
    share :: InjDict dom b a -> ASTF dom b -> m (ASTF dom a)
    share id b = do
        b' <- codeMotion hoistOver pd mkId mkSub b
        v  <- get; put (v+1)
        let x = Sym (injVariable id v)
        body <- codeMotion hoistOver pd mkId mkSub $ substitute b x a
        return
            $  Sym (injLet id)
            :$ b'
            :$ (Sym (injLambda id v) :$ body)

    descend :: AST dom b -> m (AST dom b)
    descend (lt :$ a :$ (Sym lam :$ b))
        | Just Let <- prj lt
        , Just _ <- prjLambda pd lam = do
            a' <- descend a
            b' <- codeMotion hoistOver pd mkId mkSub b
            return $ lt :$ a' :$ (Sym lam :$ b')
    descend (f :$ a) = liftM2 (:$) (descend f) (codeMotion hoistOver pd mkId mkSub a)
    descend a        = return a



fixIter :: (AlphaEq dom dom dom [(VarId, VarId)], Monad m)
    => Int
    -> (ASTF dom a -> m (ASTF dom a))
    -> (ASTF dom a -> m (ASTF dom a))
fixIter 0 f a = return a
fixIter limit f a = do
    a' <- f a
    if alphaEq a a'
      then return a
      else fixIter (limit-1) f a'



-- | Perform common sub-expression elimination and variable hoisting
codeMotion3 :: forall dom m a
    .  ( ConstrainedBy dom Typeable
       , AlphaEq dom dom dom [(VarId,VarId)]
       , Project Let dom
       , MonadState VarId m
       )
    => Int  -- Max number of iterations
    -> (forall c. ASTF dom c -> Bool)
         -- ^ Control wether a sub-expression can be hoisted over the given expression
    -> PrjDict dom
    -> MkInjDict dom
    -> MkSubEnv dom
    -> ASTF dom a
    -> m (ASTF dom a)
codeMotion3 limit hoistOver pd mkId mkSub = fixIter limit $ codeMotion hoistOver pd mkId mkSub


-- | Like 'reify' but with common sub-expression elimination and variable hoisting
reifySmart :: forall dom p pVar a
    .  ( AlphaEq dom dom (FODomain dom p pVar) [(VarId,VarId)]
       , Syntactic a
       , Domain a ~ HODomain dom p pVar
       , p :< Typeable
       , Project Let (FODomain dom p pVar)
       )
    => Int  -- Max number of iterations
    -> (forall c. ASTF (FODomain dom p pVar) c -> Bool)
    -> MkInjDict (FODomain dom p pVar)
    -> MkSubEnv (FODomain dom p pVar)
    -> a
    -> ASTF (FODomain dom p pVar) (Internal a)
reifySmart limit hoistOver mkId mkSub =
    flip evalState 0 . (codeMotion3 limit hoistOver prjDictFO mkId mkSub <=< reifyM . desugar)

