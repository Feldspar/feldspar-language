module Feldspar.Core.Middleend.Constructors
  ( Bag(..)
  , BindBag
  , AExpB
  , RExpB
  , variable
  , literal
  , lambda
  , app
  , aIn
  , mkBinds
  , toExpr
  , fromExpr
  , fromRExpr
  , unAnnotateB
  )
  where

import Feldspar.Core.UntypedRepresentation

-- | Intelligent constructors for placing bindings

data Bag a = Bags [Bag a]
           | Item a
           deriving (Eq, Ord, Show)

type BindBag a = Bag [(Var, AUntypedFeld a)]

foldBag :: (a -> b -> b) -> b -> Bag a -> b
foldBag f u (Bags bs) = foldr (\ b r -> foldBag f r b) u bs
foldBag f u (Item x) = f x u

appendBag :: Bag a -> Bag a -> Bag a
appendBag (Bags []) b         = b
appendBag b         (Bags []) = b
appendBag l         (Bags rs) = Bags $ l : rs
appendBag l         (Item r)  = Bags [l, Item r]

concatBags :: [Bag a] -> Bag a
concatBags = foldr appendBag (Bags [])

type AExpB a = (BindBag a, AUntypedFeld a)
type RExpB a = (BindBag a, RRExp a)

type RRExp a = UntypedFeldF (ATerm a UntypedFeldF)

toExpr :: AExpB a -> AUntypedFeld a
toExpr (b,e) = foldBag (curry mkLets) e b

fromExpr :: AUntypedFeld a -> AExpB a
fromExpr e = (Bags [], e)

unAnnotateB :: AExpB a -> RExpB a
unAnnotateB (b, (AIn _ e)) = (b, e)

fromRExpr :: RRExp a -> RExpB a
fromRExpr e = (Bags [], e)

variable :: Var -> RExpB a
variable v = (Bags [], Variable v)

literal :: Lit -> RExpB a
literal l = (Bags [], Literal l)

lambda :: Var -> AExpB a -> RExpB a
lambda v eb = (Bags [], Lambda v $ toExpr eb)

app :: Op -> Type -> [AExpB a] -> RExpB a
app Condition t [(b,ec), et, ee] = (b, App Condition t [ec, toExpr et, toExpr ee])
app p t [be] | p `elem` [MkFuture, ParFork] = (Bags [], App p t [toExpr be])
app op t es = (concatBags bs, App op t es1)
  where (bs,es1) = unzip es

aIn :: a -> RExpB a -> AExpB a
aIn r (b,e) = (b, AIn r e)

mkBinds :: ([(Var, AExpB a)], AExpB a) -> AExpB a
mkBinds (bs,(b,e)) = (foldr appendBag (appendBag (Item $ zip vs es1) b) bs1, e)
  where (vs,bes) = unzip bs
        (bs1,es1) = unzip bes

