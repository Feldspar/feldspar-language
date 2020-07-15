--
-- Copyright (c) 2019, ERICSSON AB
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

module Feldspar.Core.Middleend.OptimizeUntyped ( optimize ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Feldspar.Core.UntypedRepresentation
import Feldspar.Core.ValueInfo (ValueInfo(..), aLit)
import qualified Data.Bits as B
import Feldspar.Core.Middleend.FromTypeUtil (convSize)
import Feldspar.Core.Types (BitWidth(..))

-- | General simplification. Could in theory be done at earlier stages.
optimize :: AUntypedFeld ValueInfo -> AUntypedFeld ValueInfo
optimize e = deadCodeElim $ simplify M.empty e -- go empty . go empty

type AExp = AUntypedFeld ValueInfo -- ^ Annotated expressions
type UExp = UntypedFeldF AExp -- ^ Unannotated expressions
type SM = M.Map VarId AExp -- ^ Associate a let bound variable with its value

simplify :: SM -> AExp -> AExp
simplify env' (AIn r e') = simp env' e'
  where simp env (Variable v) = AIn r $ derefToA env v
        simp env (Lambda v e) = AIn r $ Lambda v $ simplify env e -- Or use mkLam
        simp _   (Literal l) = AIn r $ Literal l
        simp env (App Let _ [eRhs, eLam]) = simpLet env (simplify env eRhs) (unwrap eLam)
        -- Transform Bind to Then for values of type Unit
        simp env (App Bind t [e1, AIn _ (Lambda v e2)])
           | typeof v == TupType []
           = simpApp env r Then t [simplify env e1, simplify (extend env v eUnit) e2]
        simp env (App Condition t [ec, et, ee]) = simpCond env r t (simplify env ec) et ee
        simp env (App p t es) | p `elem` [For, EparFor]
                              = simpLoop env r p t es
        simp env (App op t es) = simpApp env r op t $ map (simplify env) es
        simp _ (LetFun (s,_,_) _) = error $ "OptimizeUntyped.simplify: LetFun "
                                             ++ s ++ " not allowed"

simpLet :: SM -> AExp -> UExp -> AExp
simpLet env rhs (Lambda v eBody)
  | App Let _ [rRhs, rBind] <- unwrap rhs
  , Lambda rv rBody <- unwrap rBind
  = mkLet rv rRhs $ simpLet (extend env rv rRhs) rBody $ Lambda v eBody
  | sharable rhs = mkLet v rhs $ simplify (extend env v rhs) eBody
  | otherwise = simplify (extend env v rhs) eBody
simpLet _ _ eLam
  = error $ "OptimizeUntyped.simpLet: malformed lambda in let: " ++ show eLam

simpCond :: SM -> ValueInfo -> Type -> AExp -> AExp -> AExp -> AExp
simpCond env r t ec@(AIn _ (Variable v)) et ee = AIn r $ App Condition t [ec, et1, ee1]
  where et1 = simplify (extend env v $ aLit $ LBool True)  et
        ee1 = simplify (extend env v $ aLit $ LBool False) ee
simpCond env r t ec et ee = simpApp env r Condition t [ec, simplify env et, simplify env ee]

simpLoop :: SM -> ValueInfo -> Op -> Type -> [AExp] -> AExp
simpLoop env r op t (eTC:es) = go op (simplify env eTC) es
  where go For tc [eSt, eLam]
         | zero tc
         = simplify env eSt
         | Literal (LInt s sz 1) <- unwrap tc
         , Lambda vIx eLam1 <- unwrap eLam
         , Lambda vSt body <- unwrap eLam1
         = simplify env $ mkLet vIx (aLit $ LInt s sz 0)
                        $ mkLet vSt eSt body
        go EparFor tc [AIn _ (Lambda vIx body)]
         | zero tc
         = AIn r $ App ESkip t []
         | Literal (LInt s sz 1) <- unwrap tc
         = simplify env $ mkLet vIx (aLit $ LInt s sz 0) body
        -- Fall through
        go op tc es = AIn r $ App op t (tc : map (simplify env) es)

simpApp :: SM -> ValueInfo -> Op -> Type -> [AExp] -> AExp
simpApp env r op' t' es = go op' t' es
  where eOrig = AIn r $ App op' t' es
        go :: Op -> Type -> [AExp] -> AExp
        go Add _ [e1, e2]
         | zero e1 = e2
         | zero e2 = e1

        go Sub _ [e1, e2]
         | zero e2 = e1

        go Mul t [e1, e2]
         | one e1 = e2
         | one e2 = e1
         | zero e1 = e1
         | zero e2 = e2
         | Just (s,n) <- uLogOf t (examine env e1) = AIn r $ App (lshift s) t [e2, n]
         | Just (s,n) <- uLogOf t (examine env e2) = AIn r $ App (lshift s) t [e1, n]

        go Div t [e1, e2]
         | one e2 = e1
         | Just (s,n) <- uLogOf t (examine env e2) = AIn r $ App (rshift s) t [e1, n]

        go Condition _ [ec, et, ee]
         | Literal (LBool b) <- unwrap ec
         = if b then et else ee

        -- A value is converted from 'typeof e1' through 't1' to 't' where
        -- the types are such that we can do the conversion directly
        go I2N t [e]
         | App I2N t1 [e1] <- examine env e
         , compatible (typeof e1) t1 t
         = simpApp env r I2N t [e1]

        go I2N t [e]
         | typeof e == t
         = e

        go op t [e]
         | op == I2N
         , Literal l <- unwrap e
         , Just e1 <- convert l t
         = e1

        -- Basic constant folder
        go op _ [e1, e2]
         | Literal l1 <- examine env e1 -- Some literals are large and not always inlined
         , Literal l2 <- examine env e2
         = constFold eOrig op l1 l2

        go op _ [e1]
         | Literal l1 <- examine env e1 -- Some literals are large and not always inlined
         = constFold1 eOrig op l1

        -- Create a 1 element long array (frequent with MultiDim) and immediately select that
        -- element. Can e seen in the metrics test in feldspar-compiler.
        -- (RunMutableArray (Bind (NewArr_ 1)
        --                        (\v3 -> Then (SetArr v3 0 e3) (Return v3)))) ! 0
        go GetIx _ [arr, AIn _ (Literal (LInt _ _ 0))]
         | (AIn _ (App RunMutableArray _ [AIn _ (App Bind _ [AIn _ (App NewArr_ _ [l]), e'])]))
           <- arr
         , one l
         , (AIn _ (Lambda v1 (AIn _ (App Then _  [sarr, ret])))) <- e'
         , (AIn _ (App SetArr _ [AIn _ (Variable v3), AIn _ (Literal (LInt _ _ 0)), e3]))
           <- sarr
         , (AIn _ (App Return _ [AIn _ (Variable v2)])) <- ret
         , v1 == v2
         , v1 == v3
         = e3

        -- Same rule as previous rule but with Elements as backing write.
        go GetIx _ [arr, AIn _ (Literal (LInt _ _ n))]
         | AIn _ (App EMaterialize _ [AIn _ Literal{}, e@(AIn _ (App EPar _ _))]) <- arr
         , Just e3 <- grabWrite n e
         = e3

        -- GetLength applied to an array construction
        go GetLength _ [arr]
         | App EMaterialize _ [eLen, _] <- examine env arr
         = eLen

        -- Select from a tuple expression
        go (Sel n) _ [eTup]
         | App Tup _ es <- examine env eTup
         , n < length es
         , eComp <- es !! n
         , not $ sharable eComp
         = eComp

        -- Select from a tuple literal
        go (Sel n) _ [eTup]
         | Literal (LTup es) <- examine env eTup
         , n < length es
         = aLit $ es !! n

        -- Tuple copy
        go Tup t es
         | (e:es1) <- map (examine env) es
         , App (Sel 0) _ [eTup] <- e
         , typeof eTup == t
         , and $ zipWith (check eTup) es1 [1 ..]
         = eTup
           where check eTup (App (Sel n) _ [e]) i = n == i && e == eTup
                 check _    _              _ = False

        -- Fall through
        go _ _ _ = eOrig

uLogOf :: Type -> UExp -> Maybe (Signedness, AExp)
uLogOf (_ :# IntType sgn sz) (Literal (LInt sgn' sz' n))
  | sz == convSize NNative
  , n > 0
  , Just m <- intLog2 n
  , sgn == sgn' && sz == sz'
  = Just (sgn, aLit $ LInt sgn sz m)
uLogOf _ _ = Nothing

intLog2 :: Integer -> Maybe Integer
intLog2 = il 0
  where il m 1 = Just m
        il m n | even n = il (m+1) (n `div` 2)
               | otherwise = Nothing

lshift :: Signedness -> Op
lshift Unsigned = ShiftLU
lshift Signed   = ShiftL

rshift :: Signedness -> Op
rshift Unsigned = ShiftRU
rshift Signed   = ShiftR

convert :: Lit -> Type -> Maybe AExp
convert (LInt _ _ n) (1 :# IntType s2 sz2) = Just $ aLit $ LInt s2 sz2 n
convert (LInt _ _ n) (1 :# FloatType)
  = Just $ aLit $ LFloat $ fromIntegral n
convert (LInt _ _ n) (1 :# DoubleType)
  = Just $ aLit $ LDouble $ fromIntegral n
convert _ _ = Nothing

compatible :: Type -> Type -> Type -> Bool
compatible (n1 :# IntType s1 sz1) (n2 :# IntType s2 sz2) (n3 :# IntType _ sz3)
  | n1 == n2 && n2 == n3 = sz2 >= sz3 || s1 == s2 && sz1 <= sz2
compatible _ _ _ = False

extend :: SM -> Var -> AExp -> SM
extend env v e = M.insert (varNum v) e env

deref :: SM -> Var -> UExp
deref env v = maybe (Variable v) (examine env) $ M.lookup (varNum v) env

derefToA :: SM -> Var -> UExp
derefToA env v = case M.lookup (varNum v) env of
                   Just (AIn _ (Variable v')) -> derefToA env v'
                   Just e@(AIn _ ue) | not $ sharable e -> ue
                   _ -> Variable v

examine :: SM -> AExp -> UExp
examine env (AIn _ (Variable v)) = deref env v
examine _   (AIn _ e)            = e

unwrap :: AExp -> UExp
unwrap (AIn _ e) = e

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

-- | Simple constant folder that returns result or the original expression
-- | Num, Ord and Eq operations
constFold :: AUntypedFeld ValueInfo -> Op -> Lit -> Lit -> AUntypedFeld ValueInfo
constFold _ Add (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (n1 + n2))
constFold _ Sub (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (n1 - n2))
constFold _ Mul (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (n1 * n2))
constFold _ Min (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (min n1 n2))
constFold _ Max (LInt sz n n1) (LInt _ _ n2) = aLit (LInt sz n (max n1 n2))
constFold _ LTH (LInt _  _ n1) (LInt _ _ n2) = aLit (LBool $ n1 < n2)
constFold _ LTE (LInt _  _ n1) (LInt _ _ n2) = aLit (LBool $ n1 <= n2)
constFold _ GTH (LInt _  _ n1) (LInt _ _ n2) = aLit (LBool $ n1 > n2)
constFold _ GTE (LInt _  _ n1) (LInt _ _ n2) = aLit (LBool $ n1 >= n2)
constFold _ Equal (LInt _ _ n1) (LInt _ _ n2) = aLit (LBool $ n1 == n2)
constFold _ NotEqual (LInt _ _ n1) (LInt _ _ n2) = aLit (LBool $ n1 /= n2)

constFold _ Add (LFloat x) (LFloat y) = aLit (LFloat $ x + y)
constFold _ Sub (LFloat x) (LFloat y) = aLit (LFloat $ x - y)
constFold _ Mul (LFloat x) (LFloat y) = aLit (LFloat $ x * y)
constFold _ Min (LFloat x) (LFloat y) = aLit (LFloat $ min x y)
constFold _ Max (LFloat x) (LFloat y) = aLit (LFloat $ max x y)
constFold _ LTH (LFloat x) (LFloat y) = aLit (LBool $ x < y)
constFold _ LTE (LFloat x) (LFloat y) = aLit (LBool $ x <= y)
constFold _ GTH (LFloat x) (LFloat y) = aLit (LBool $ x > y)
constFold _ GTE (LFloat x) (LFloat y) = aLit (LBool $ x >= y)
constFold _ Equal (LFloat x) (LFloat y) = aLit (LBool $ x == y)
constFold _ NotEqual (LFloat x) (LFloat y) = aLit (LBool $ x /= y)

constFold _ Add (LDouble x) (LDouble y) = aLit (LDouble $ x + y)
constFold _ Sub (LDouble x) (LDouble y) = aLit (LDouble $ x - y)
constFold _ Mul (LDouble x) (LDouble y) = aLit (LDouble $ x * y)
constFold _ Min (LDouble x) (LDouble y) = aLit (LDouble $ min x y)
constFold _ Max (LDouble x) (LDouble y) = aLit (LDouble $ max x y)
constFold _ LTH (LDouble x) (LDouble y) = aLit (LBool $ x < y)
constFold _ LTE (LDouble x) (LDouble y) = aLit (LBool $ x <= y)
constFold _ GTH (LDouble x) (LDouble y) = aLit (LBool $ x > y)
constFold _ GTE (LDouble x) (LDouble y) = aLit (LBool $ x >= y)
constFold _ Equal (LDouble x) (LDouble y) = aLit (LBool $ x == y)
constFold _ NotEqual (LDouble x) (LDouble y) = aLit (LBool $ x /= y)

-- | Bit operations
constFold _ BAnd (LInt sgn1 sz1 n1) (LInt _ _ n2) = aLit (LInt sgn1 sz1 $ n1 B..&. n2)
constFold _ BOr  (LInt sgn1 sz1 n1) (LInt _ _ n2) = aLit (LInt sgn1 sz1 $ n1 B..|. n2)
constFold _ BXor (LInt sgn1 sz1 n1) (LInt _ _ n2) = aLit (LInt sgn1 sz1 $ B.xor n1 n2)

constFold _ GetIx (LArray _ ls) (LInt _ _ n)
  | n >= 0
  , fromInteger n < length ls
  = aLit $ ls !! fromInteger n

constFold e _ _ _ = e

constFold1 :: AUntypedFeld ValueInfo -> Op -> Lit -> AUntypedFeld ValueInfo
constFold1 e GetLength (LArray _ xs)
  | 1 :# IntType s n <- typeof e = aLit (LInt s n $ fromIntegral $ length xs)
constFold1 e _ _ = e

-- | Scan an Epar/Ewrite-nest and return the element written to a position.
grabWrite :: Integer -> AUntypedFeld a -> Maybe (AUntypedFeld a)
grabWrite n (AIn _ (App EPar _ [e1,e2]))
 | Nothing <- r1 = grabWrite n e2
 | otherwise = r1
   where r1 = grabWrite n e1
grabWrite n (AIn _ (App EWrite _ [AIn _ (Literal (LInt _ _ k)), e]))
 | k == n = Just e
grabWrite _ _ = Nothing

mkLet :: Var -> AExp -> AExp -> AExp
mkLet v rhs body = mkLets ([(v,rhs)], body)

eUnit :: AUntypedFeld ValueInfo
eUnit = aLit $ LTup []

-- We need to eliminate dead bindings

deadCodeElim :: AExp -> AExp
deadCodeElim = snd . dceA
  where dceA (AIn r e) = let (vs,e1) = dceU e in (vs, AIn r e1)
        dceA :: AExp -> (S.Set Var, AExp)
        dceU :: UExp -> (S.Set Var, UExp)
        dceU (Variable v) = (S.singleton v, Variable v)
        dceU (Literal l) = (S.empty, Literal l)
        dceU (App Let t [eRhs, AIn r (Lambda v eBody)]) = dcLet t (dceA eRhs) r v $ dceA eBody
        dceU (App Save _ [AIn _ e]) = dceU e
        dceU (App op t es) = let (vss,es1) = unzip $ map dceA es
                              in (S.unions vss, App op t es1)
        dceU (Lambda v e) = let (vs,e1) = dceA e
                             in (v `S.delete` vs, Lambda v e1)
        dceU e = error $ "OptimizeUntyped.deadCodeElim: illegal expression " ++ show e
        dcLet t (vsR,eR) r v (vsB,eB)
            | S.member v vsB = (vsR `S.union` (v `S.delete` vsB),
                                App Let t [eR, AIn r $ Lambda v eB])
            | otherwise = (vsB, unwrap eB)
