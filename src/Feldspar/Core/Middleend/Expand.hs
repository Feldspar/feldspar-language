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
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wall #-}
-- FIXME: This code is inherently tricky, stop shadowing variables to avoid
--        more confusion.
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- FIXME: Annotate the types in the relevant places.
{-# OPTIONS_GHC -Wno-type-defaults #-}
-- FIXME: Fix the incomplete patterns.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Handle situations where an expression in a loop nest depends on the innermost loop
-- (so ordinary loop invariant removal does not help) but is invariant with respect to
-- some outer loop. To this end we expand the value to an array with dimensions as the
-- inner loops the expression depends on. The (array) expresion can now be floated
-- out of the inner loops (because of the array expansion) as well as the outer invariant
-- loop(s).
-- This transformation is important since the unrestricted inling by the vector library
-- in combination with the embedding mechanism often creates this kind of loop nests.

module Feldspar.Core.Middleend.Expand (expand) where

import Feldspar.Core.UntypedRepresentation
import Feldspar.Core.ValueInfo (ValueInfo, aLit, elementsVI, setLB, addVI,
                                mulVI)
import Feldspar.Lattice (top)

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List (partition)

type VarMap = M.Map Var (S.Set Var, RExp)
type UExp = UntypedFeld ValueInfo
type RExp = UntypedFeldF UExp

expand :: UExp -> UExp
expand e = snd . snd $ evalState (expE [] M.empty e) 10000

data AbsInfo = LoopI {trip :: (S.Set Var, UExp), ixVar :: Var, absVars :: S.Set Var}
             | AbsI  {absVars :: S.Set Var}
             deriving Show

data BindInfo = BI {biAbsI :: [AbsInfo], biBindIs :: [BindInfo], biBind :: (Var, UExp)}
              deriving Show

expE :: [AbsInfo] -> VarMap -> UExp -> Rename (S.Set Var, ([BindInfo], UExp))
expE ai vm e = do (s,bse) <- expF ai vm e
                  return (s, if null ai && not (null $ fst bse)
                                then error $ "Found bs = " ++ show (fst bse)
                                else bse)

expF :: [AbsInfo] -> VarMap -> UExp -> Rename (S.Set Var, ([BindInfo], UExp))
expF ai vm e = mdo (s, (bs1,e1)) <- ea aiNew vm e
                   let (bs2,e2) = catch bs1 e1
                       ai1 = if simpleExp ai vm e then [] else ai
                       profLoops = profitableLoops $ legalLoops s ai1
                       keepLoops = [ai | (False, ai@LoopI{}) <- profLoops]
                       aiNew = keepLoops ++ drop (length profLoops) ai1
                       (idxE, lenE) = idxLenExpr keepLoops
                       r = elementsVI (setLB 0 $ getAnnotation idxE) (getAnnotation e)
                       eBody = In r $ App EWrite (ElementsType $ typeof e) [idxE, e2]
                       (bs3, e3) = foldl mkBinds (bs2, eBody) keepLoops
                       flArr = In r $ App EMaterialize (ArrayType top $ typeof e) [lenE, e3]
                   arrV <- newVar $ Var 0 (typeof flArr) $ B.pack "a"
                   let arrVE = In r $ Variable arrV
                       b = BI {biAbsI = map snd profLoops, biBindIs = bs3, biBind = (arrV, flArr)}
                       refE = In (getAnnotation e) $ App GetIx (typeof e) [arrVE, idxE]
                   return (s, if null keepLoops || simpleExp ai vm e1
                                 then (bs2, e2) -- We should not float e
                                 else ([b], refE)) -- We float an expanded e in b

ea :: [AbsInfo] -> VarMap -> UExp -> Rename (S.Set Var, ([BindInfo], UExp))
ea ai vm (In r e) = do (s,(bs, e1)) <- eu ai vm e
                       return (s, (bs, In r e1))

-- | Construct an index expression and length expression
-- Absinfos are innermost first
idxLenExpr :: [AbsInfo] -> (UExp, UExp)
idxLenExpr ai = ixTop $ reverse ai
  where ixTop (LoopI{trip = (_,tc), ixVar = v} : ai) = ixE (var tc v) tc ai
        ixTop [] = (aLit $ LInt Unsigned S32 0, aLit $ LInt Unsigned S32 0)
        ixE e s (LoopI{trip = (_,tc), ixVar = v} : ai) = ixE (mul e tc `add` var tc v) (mul s tc) ai
        ixE e s [] = (e,s)
        var e v = In (setLB 0 $ getAnnotation e) $ Variable v
        add x y = In (addVI (getAnnotation x) (getAnnotation y)) $ App Add (typeof x) [x, y]
        mul x y = In (mulVI (getAnnotation x) (getAnnotation y)) $ App Mul (typeof x) [x, y]

mkBinds :: ([BindInfo], UntypedFeld ValueInfo) -> AbsInfo -> ([BindInfo], UExp)
mkBinds (bs, e) LoopI{trip = (_,eTrip), ixVar = v} = catch (shiftBIs bs) loopE
  where loopE = In r $ App EparFor (typeof e) [eTrip, In r $ Lambda v e]
        r = getAnnotation e


shiftBIs :: [BindInfo] -> [BindInfo]
shiftBIs bs = [b{biAbsI = tail $ biAbsI b} | b <- bs]

-- Bindings are outermost first
-- Absinfos are innermost first
catch :: [BindInfo] -> UExp -> ([BindInfo], UExp)
catch bs e = (fbs ++ concatMap biBindIs pbs, mkLets (map biBind pbs, e))
  where (pbs,fbs) = partition (null . biAbsI) bs

legalLoops :: S.Set Var -> [AbsInfo] -> [(Bool, AbsInfo)]
legalLoops vs (a@AbsI{absVars = avs} : ais)
    | S.disjoint vs avs = (False, a) : legalLoops vs ais
legalLoops vs (a@LoopI{absVars = avs, ixVar = ixv, trip = (trvs,_)} : ais)
    | S.disjoint vs avs = (S.notMember ixv vs, a) : legalLoops vs1 ais
  where vs1 = trvs `S.union` (ixv `S.delete` vs)
legalLoops _  _ = []

profitableLoops :: [(Bool, b)] -> [(Bool, b)]
profitableLoops aais = reverse $ dropWhile (not . fst) $ reverse aais

{-

Lazy binding
------------

The eu function is intended to be used in a way where computing the second component of
the return value depends indirectly on the first component. Hence it is important the
the first component (a set of free variables) can be computed without reducing the
second component to whnf. Therefore the pattern for the second component (which is itself
a pair) can not be inlined into the outer pattern in the recursive invocations.

-}

eu :: [AbsInfo] -> VarMap -> RExp -> Rename (S.Set Var, ([BindInfo], RExp))
eu _ vm (Variable v) = let (s,e) = vm M.! v in return (s, ([],e))
eu _ _ (Literal l) = return (S.empty, ([], Literal l))
eu ai vm (App Let t [eRhs, In r (Lambda v eBody)])
  = mdo (fvsR, bseR) <- expE ai vm eRhs
        let (bsR, eR) = bseR -- Note [Lazy binding]
            tryInline = simpleExp ai vm eR
            doInline = tryInline && not (null bsB) -- Inline only if we generate any bindings
            eR1 = if doInline then dropAnnotation eR else Variable v
            vmB = M.insert v (fvsR, eR1) vm
            aiB = if tryInline then ai else AbsI (S.singleton v) : ai
        (fvsB, bseB) <- expE aiB vmB eBody
        let (bsB, eB) = bseB
            eNew = App Let t [eR, In r $ Lambda v eB]
            bsB1 = if tryInline then bsB else shiftBIs bsB
        return (fvsB, (bsR ++ bsB1, if doInline then dropAnnotation eB else eNew))
eu ai vm (App op t [eLen, eInit, In r1 (Lambda vIx (In r2 (Lambda vSt eBody)))])
  | op `elem` [ForLoop, Sequential]
  = do (fvsL, bseL) <- expE ai vm eLen
       let (bsL, eL) = bseL
       (fvsI, bseI) <- expE ai vm eInit
       let (bsI, eI) = bseI
       -- Maybe the trip count should be bound to a variable
       let aiB = LoopI {trip = (fvsL,eL), ixVar = vIx, absVars = S.singleton vSt} : ai
           vmB = M.insert vIx (S.singleton vIx, Variable vIx) $
                 M.insert vSt (S.singleton vSt, Variable vSt) vm
       (fvsB, bseB) <- expE aiB vmB eBody
       let (bsB, eB) = bseB
       return (fvsL `S.union` fvsI `S.union` (fvsB S.\\ S.fromList [vIx, vSt]),
               (bsL ++ bsI ++ shiftBIs bsB,
                App op t [eL, eI, In r1 $ Lambda vIx $ In r2 $ Lambda vSt eB]))
eu ai vm (App op t [eLen, In r1 (Lambda vIx eBody)])
  | op `elem` [Parallel, EparFor, For]
  = do (fvsL, bseL) <- expE ai vm eLen
       let (bsL, eL) = bseL
       -- Maybe the trip count should be bound to a variable
       let aiB = LoopI {trip = (fvsL,eL), ixVar = vIx, absVars = S.empty} : ai
           vmB = M.insert vIx (S.singleton vIx, Variable vIx) vm
       (fvsB, bseB) <- expE aiB vmB eBody
       let (bsB, eB) = bseB
       return (fvsL `S.union` (vIx `S.delete` fvsB),
               (bsL ++ shiftBIs bsB,
                App op t [eL, In r1 $ Lambda vIx eB]))
eu ai vm (App Condition t (e:es))
  = do fvbse0 <- expE [] vm e
       fvbseR <- mapM (expE ai vm) es
       let fvbses = fvbse0 : fvbseR
           (fvss, bses) = unzip fvbses
           (bss, es1) = unzip bses
       return (S.unions fvss, (concat bss, App Condition t es1))
eu ai vm (App op t es)
  = do fvbses <- mapM (expE ai vm) es
       let (fvss,bses) = unzip fvbses
           (bss, es1) = unzip bses
       return (S.unions fvss, (concat bss, App op t es1))
eu ai vm (Lambda v e)
  = do let vs = S.singleton v
       (fvs,bse) <- expE (AbsI{absVars = vs} : ai) (M.insert v (vs, Variable v) vm) e
       return (fvs S.\\ vs, (shiftBIs $ fst bse, Lambda v $ snd bse))

simpleExp :: [AbsInfo] -> VarMap -> UExp -> Bool
simpleExp ai vm e = not (goodToShare e) || simpleArrRef e || expCost ai vm e <= 2

expCost :: [AbsInfo] -> VarMap -> UExp -> Int
expCost ai vm (In _ e) = go e
  where go (App op _ es) = appCost ai vm op es
        go _             = 5

appCost :: [AbsInfo] -> VarMap -> Op -> [UExp] -> Int
appCost ai vm = go
  where go Mul [In _ (Variable v), In _ (Variable u)]
           | ixAndInv ai v (fst $ vm M.! u) = 1
        go ShiftLU [In _ (Variable _), In _ (Literal _)] = 1
        go _   _                            = 5

ixAndInv :: [AbsInfo] -> Var -> S.Set Var -> Bool
ixAndInv ai v vs = go ai
  where go (LoopI{ixVar = ix, absVars = avs} : ai) =
          S.disjoint vvs avs && (ix == v || go ai)
        go (AbsI{absVars = avs} : ai) = S.disjoint vvs avs && go ai
        go [] = True
        vvs = v `S.insert` vs

-- | Check that an expression is a simple array reference that we can inline
simpleArrRef :: UExp -> Bool
simpleArrRef (In _ (App GetIx _ [In _ (Variable _), e])) = simpleIdxE e
  where simpleIdxE (In _ (Variable _)) = True
        simpleIdxE (In _ (App op _ es)) = op `elem` [Add,Sub,Mul] && all simpleIdxE es
        simpleIdxE _ = False
simpleArrRef _ = False
