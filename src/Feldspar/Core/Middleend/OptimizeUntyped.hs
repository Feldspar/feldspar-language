module Feldspar.Core.Middleend.OptimizeUntyped ( optimize ) where

import Data.Map.Strict (Map, empty)
import Feldspar.Core.UntypedRepresentation
import Feldspar.ValueInfo (ValueInfo(..))

-- | General simplification. Could in theory be done at earlier stages.
optimize :: AUntypedFeld ValueInfo -> AUntypedFeld ValueInfo
optimize = go empty . go empty

go :: Map k v -> AUntypedFeld ValueInfo -> AUntypedFeld ValueInfo
go _ e@(AIn _ Variable{}) = e
go env (AIn r (Lambda v e)) = AIn r (Lambda v (go env e))
go env (AIn r (LetFun (s, f, e1) e2)) = AIn r (LetFun (s, f, go env e1) (go env e2))
go _ l@(AIn _ Literal{}) = l

go env (AIn _ (App Let _ [e1, AIn _ (Lambda x body)]))
 | (AIn _ Variable{}) <- e1 -- let x = y in e ==> [y/x]e
 = go env $ subst e1 x body
 | linear x body
 = go env $ subst e1 x body

go env (AIn _ (App Add _ [e1, e2]))
 | zero e1 = go env e2
 | zero e2 = go env e1

go env (AIn _ (App Sub _ [e1, e2]))
 | zero e2 = go env e1

go env (AIn _ (App Mul _ [e1, e2]))
 | zero e1 = e1
 | zero e2 = e2
 | one e1  = go env e2
 | one e2  = go env e1

go env (AIn _ (App Div _ [e1, e2]))
 | one e2  = go env e1

-- Basic constant folder.
go _ e@(AIn _ (App p _ [AIn _ (Literal l1), AIn _ (Literal l2)]))
  | p `elem` [Add, Sub, Mul]
  = constFold e p l1 l2

-- For 1 (\v -> body) ==> [0/v]body
go env (AIn _ (App p _ [AIn _ (Literal (LInt s sz 1)), AIn _ (Lambda v body)]))
  | p `elem` [For, EparFor]
  = go env $ subst (aLit (LInt s sz 0)) v body

-- Create a 1 element long array (frequent with MultiDim) and immediately select that
-- element. Can e seen in the metrics test in feldspar-compiler.
-- (RunMutableArray (Bind (NewArr_ 1)
--                        (\v3 -> Then (SetArr v3 0 e3) (Return v3)))) ! 0
go env (AIn _ (App GetIx _ [arr, AIn _ (Literal (LInt _ _ 0))]))
 | (AIn _ (App RunMutableArray _ [AIn _ (App Bind _ [AIn _ (App NewArr_ _ [l]), e'])]))
   <- arr
 , one l
 , (AIn _ (Lambda v1 (AIn _ (App Then _  [sarr, ret])))) <- e'
 , (AIn _ (App SetArr _ [AIn _ (Variable v3), AIn _ (Literal (LInt _ _ 0)), e3]))
   <- sarr
 , (AIn _ (App Return _ [AIn _ (Variable v2)])) <- ret
 , v1 == v2
 , v1 == v3 = go env e3

-- Same rule as previous rule but with Elements as backing write.
go env (AIn _ (App GetIx _ [arr, AIn _ (Literal (LInt _ _ n))]))
 | AIn _ (App EMaterialize _ [AIn _ Literal{}, e@(AIn _ (App EPar _ _))]) <- arr
 , Just e3 <- grabWrite n e = go env e3

-- Tuple selections, 1..15. Deliberately avoiding take 1 . drop k which will
-- result in funny things with broken input.
go env (AIn _ (App Sel1 _  [AIn _ (App Tup _ (e:_))]))
  = go env e
go env (AIn _ (App Sel2 _  [AIn _ (App Tup _ (_:e:_))]))
  = go env e
go env (AIn _ (App Sel3 _  [AIn _ (App Tup _ (_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel4 _  [AIn _ (App Tup _ (_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel5 _  [AIn _ (App Tup _ (_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel6 _  [AIn _ (App Tup _ (_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel7 _  [AIn _ (App Tup _ (_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel8 _  [AIn _ (App Tup _ (_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel9 _  [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel10 _ [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel11 _ [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel12 _ [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel13 _ [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel14 _ [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e
go env (AIn _ (App Sel15 _ [AIn _ (App Tup _ (_:_:_:_:_:_:_:_:_:_:_:_:_:_:e:_))]))
  = go env e

-- Fallthrough.
go env (AIn r (App p t es)) = AIn r (App p t $ map (go env) es)

linear :: Var -> AUntypedFeld a -> Bool
linear v e = count v e <= 1

-- | Occurence counter. Cares about dynamic behavior, so loops count as a lot.
count :: Var -> AUntypedFeld a -> Integer
count v (AIn _ (Variable v')) = if v == v' then 1 else 0
count v e@(AIn _ (Lambda v' _))
  | v == v' || v `notElem` fvA e = 0
  | otherwise                  = 100 -- Possibly inside loop
count v (AIn _ (LetFun (_, _, e1) e2)) = count v e1 + count v e2
count _ (AIn _ Literal{}) = 0
count v (AIn _ (App Let _ [e1, AIn _ (Lambda x body)]))
  | v == x    = count v e1
  | otherwise = count v e1 + count v body
count _ (AIn _ (App Await _ _)) = 100 -- Do not inline.
count _ (AIn _ (App NoInline _ _)) = 100 -- Do not inline.
count v (AIn _ (App _ _ es)) = sum $ map (count v) es

-- TODO: Improve precision of switch.

-- | Is this a literal zero.
zero :: AUntypedFeld a -> Bool
zero (AIn _ (Literal (LInt    _ _ 0))) = True
zero (AIn _ (Literal (LFloat      0))) = True
zero (AIn _ (Literal (LDouble     0))) = True
zero _                                 = False

-- | Is this a literal one.
one :: AUntypedFeld a -> Bool
one (AIn _ (Literal (LInt    _ _ 1))) = True
one (AIn _ (Literal (LFloat      1))) = True
one (AIn _ (Literal (LDouble     1))) = True
one _                                 = False

constFold :: AUntypedFeld ValueInfo -> Op -> Lit -> Lit -> AUntypedFeld ValueInfo
constFold _ Add (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (n1 + n2))
constFold _ Sub (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (n1 - n2))
constFold _ Mul (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (n1 * n2))
constFold e _ _ _ = e

-- | Scan an Epar/Ewrite-nest and return the element written to a position.
grabWrite :: Integer -> AUntypedFeld a -> Maybe (AUntypedFeld a)
grabWrite n (AIn _ (App EPar _ [e1,e2]))
 | Nothing <- r1 = grabWrite n e2
 | otherwise = r1
   where r1 = grabWrite n e1
grabWrite n (AIn _ (App EWrite _ [AIn _ (Literal (LInt _ _ k)), e]))
 | k == n = Just e
grabWrite _ _ = Nothing
