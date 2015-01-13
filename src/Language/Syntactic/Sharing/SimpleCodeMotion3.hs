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
    ( codeMotion3
    , reifySmart
    ) where



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



-- | A sub-expression chosen to be shared together with an evidence that it can actually be shared
-- in the whole expression under consideration
data Chosen dom a
  where
    Chosen
      :: Int  -- Size
      -> InjDict dom b a -> ASTF dom b -> Chosen dom a

-- | Choose between two chosen expressions. If both are defined, the largest one is chosen.
pick :: Maybe (Chosen dom a) -> Maybe (Chosen dom a) -> Maybe (Chosen dom a)
pick Nothing b = b
pick a Nothing = a
pick a@(Just (Chosen sza _ _)) b@(Just (Chosen szb _ _))
    | sza > szb = a
    | otherwise = b

-- | Choose a sub-expression to share
choose :: forall dom a
    .  (AlphaEq dom dom dom [(VarId,VarId)], Project Let dom)
    => (forall c. ASTF dom c -> Bool)
    -> PrjDict dom
    -> MkInjDict dom
    -> ASTF dom a
    -> Maybe (Chosen dom a)
choose hoistOver pd mkId a = chooseEnvSub initEnv a
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
            return $ Chosen (size b) id b
        that = do
            guard $ hoistOver b
            chooseEnvSub env b

    -- | Like 'chooseEnv', but does not consider the top expression for sharing
    chooseEnvSub :: Env dom -> AST dom b -> Maybe (Chosen dom a)
    chooseEnvSub env (Sym lam :$ b)
        | Just v <- prjLambda pd lam
        = chooseEnv (env' v) b
      where
        env' v = env
            { inLambda     = True
            , dependencies = insert v (dependencies env)
            }
    chooseEnvSub env (s :$ b) = chooseEnvSub env s `pick` chooseEnv env b
    chooseEnvSub _ _ = Nothing



-- | Perform common sub-expression elimination and variable hoisting
codeMotion3 :: forall dom a
    .  ( ConstrainedBy dom Typeable
       , AlphaEq dom dom dom [(VarId,VarId)]
       , Project Let dom
       )
    => (forall c. ASTF dom c -> Bool)  -- ^ Control wether a sub-expression can be hoisted over the given expression
    -> PrjDict dom
    -> MkInjDict dom
    -> ASTF dom a
    -> State VarId (ASTF dom a)
codeMotion3 hoistOver pd mkId a
    | Just (Chosen _ id b) <- choose hoistOver pd mkId a = share id b
    | otherwise = descend a
  where
    share :: InjDict dom b a -> ASTF dom b -> State VarId (ASTF dom a)
    share id b = do
        b' <- codeMotion3 hoistOver pd mkId b
        v  <- get; put (v+1)
        let x = Sym (injVariable id v)
        body <- codeMotion3 hoistOver pd mkId $ substitute b x a
        return
            $  Sym (injLet id)
            :$ b'
            :$ (Sym (injLambda id v) :$ body)

    descend :: AST dom b -> State VarId (AST dom b)
    descend (f :$ a) = liftM2 (:$) (descend f) (codeMotion3 hoistOver pd mkId a)
    descend a        = return a



-- | Like 'reify' but with common sub-expression elimination and variable hoisting
reifySmart :: forall dom p pVar a
    .  ( AlphaEq dom dom (FODomain dom p pVar) [(VarId,VarId)]
       , Syntactic a
       , Domain a ~ HODomain dom p pVar
       , p :< Typeable
       , Project Let (FODomain dom p pVar)
       )
    => (forall c. ASTF (FODomain dom p pVar) c -> Bool)
    -> MkInjDict (FODomain dom p pVar)
    -> a
    -> ASTF (FODomain dom p pVar) (Internal a)
reifySmart hoistOver mkId =
    flip evalState 0 . (codeMotion3 hoistOver prjDictFO mkId <=< reifyM . desugar)

