{-# LANGUAGE RecursiveDo #-}

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

module Feldspar.Core.Middleend.Expand (expand) where

import Feldspar.Core.UntypedRepresentation
import Feldspar.ValueInfo
import Feldspar.Lattice (top)

import qualified Data.ByteString.Char8 as B
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.List (partition)

type VarMap = M.Map Var (S.Set Var, RExp)
type UExp = AUntypedFeld ValueInfo
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
                       eBody = AIn r $ App EWrite (ElementsType $ typeof e) [idxE, e2]
                       (bs3, e3) = foldl mkBinds (bs2, eBody) keepLoops
                       flArr = AIn r $ App EMaterialize (ArrayType top $ typeof e) [lenE, e3]
                   arrV <- newVar $ Var 0 (typeof flArr) $ B.pack "a"
                   let arrVE = AIn r $ Variable arrV
                       b = BI {biAbsI = map snd profLoops, biBindIs = bs3, biBind = (arrV, flArr)}
                       refE = AIn (getAnnotation e) $ App GetIx (typeof e) [arrVE, idxE]
                   return (s, if null keepLoops || not (sharable e1) || simpleExp ai vm e1
                                 then (bs2, e2) -- We should not float e
                                 else ([b], refE)) -- We float an expanded e in b

ea :: [AbsInfo] -> VarMap -> UExp -> Rename (S.Set Var, ([BindInfo], UExp))
ea ai vm (AIn r e) = do (s,(bs,e1)) <- eu ai vm e
                        return (s, (bs, AIn r e1))

-- | Construct an index expression and length expression
-- Absinfos are innermost first
idxLenExpr :: [AbsInfo] -> (UExp, UExp)
idxLenExpr ai = ixTop $ reverse ai
  where ixTop (LoopI{trip = (_,tc), ixVar = v} : ai) = ixE (var tc v) tc ai
        ixTop [] = (aLit $ LInt Unsigned S32 0, aLit $ LInt Unsigned S32 0)
        ixE e s (LoopI{trip = (_,tc), ixVar = v} : ai) = ixE (mul e tc `add` var tc v) (mul s tc) ai
        ixE e s [] = (e,s)
        var e v = AIn (setLB 0 $ getAnnotation e) $ Variable v
        add x y = AIn (addVI (getAnnotation x) (getAnnotation y)) $ App Add (typeof x) [x, y]
        mul x y = AIn (mulVI (getAnnotation x) (getAnnotation y)) $ App Mul (typeof x) [x, y]

mkBinds (bs, e) LoopI{trip = (_,eTrip), ixVar = v} = catch (shiftBIs bs) loopE
  where loopE = AIn r $ App EparFor (typeof e) [eTrip, AIn r $ Lambda v e]
        r = getAnnotation e


shiftBIs :: [BindInfo] -> [BindInfo]
shiftBIs bs = [b{biAbsI = tail $ biAbsI b} | b <- bs]

-- Bindings are outermost first
-- Absinfos are innermost first
catch :: [BindInfo] -> UExp -> ([BindInfo], UExp)
catch bs e = (fbs ++ concatMap biBindIs pbs, mkLets (map biBind pbs, e))
  where (pbs,fbs) = partition (null . biAbsI) bs

legalLoops vs (a@AbsI{absVars = avs} : ais)
    | disjoint vs avs = (False, a) : legalLoops vs ais
legalLoops vs (a@LoopI{absVars = avs, ixVar = ixv, trip = (trvs,_)} : ais)
    | disjoint vs avs = (not $ S.member ixv vs, a) : legalLoops vs1 ais
  where vs1 = S.union trvs (vs S.\\ S.singleton ixv)
legalLoops _  _ = []

profitableLoops aais = reverse $ dropWhile (not . fst) $ reverse aais


eu :: [AbsInfo] -> VarMap -> RExp -> Rename (S.Set Var, ([BindInfo], RExp))
eu ai vm (Variable v) = let (s,e) = vm M.! v in return (s, ([],e))
eu ai vm (Literal l) = return (S.empty, ([], Literal l))
eu ai vm (App Let t [eRhs, AIn r (Lambda v eBody)])
  = do (fvsR, (bsR, eR)) <- expE ai vm eRhs
       let inline = not (sharable eR) || simpleArrRef eR
           eR1 = if inline then dropAnnotation eR else Variable v
           vmB = M.insert v (fvsR, eR1) vm
           aiB = if inline then ai else AbsI (S.singleton v) : ai
       (fvsB, (bsB, eB)) <- expE aiB vmB eBody
       let eNew = App Let t [eR, AIn r $ Lambda v eB]
           bsB1 = if inline then bsB else shiftBIs bsB
       return (fvsB, (bsR ++ bsB1, if inline then dropAnnotation eB else eNew))
eu ai vm (App op t [eLen, eInit, AIn r1 (Lambda vIx (AIn r2 (Lambda vSt eBody)))])
  | op `elem` [ForLoop, Sequential]
  = do (fvsL, (bsL, eL)) <- expE ai vm eLen
       (fvsI, (bsI, eI)) <- expE ai vm eInit
       -- Maybe the trip count should be bound to a variable
       let aiB = LoopI {trip = (fvsL,eL), ixVar = vIx, absVars = S.singleton vSt} : ai
           vmB = M.insert vIx (S.singleton vIx, Variable vIx) $
                 M.insert vSt (S.singleton vSt, Variable vSt) vm
       (fvsB, (bsB, eB)) <- expE aiB vmB eBody
       return (fvsL `S.union` fvsI `S.union` (fvsB S.\\ S.fromList [vIx, vSt]),
               (bsL ++ bsI ++ shiftBIs bsB,
                App op t [eL, eI, AIn r1 $ Lambda vIx $ AIn r2 $ Lambda vSt eB]))
eu ai vm (App op t [eLen, AIn r1 (Lambda vIx eBody)])
  | op `elem` [Parallel, EparFor, For]
  = do (fvsL, (bsL, eL)) <- expE ai vm eLen
       -- Maybe the trip count should be bound to a variable
       let aiB = LoopI {trip = (fvsL,eL), ixVar = vIx, absVars = S.empty} : ai
           vmB = M.insert vIx (S.singleton vIx, Variable vIx) vm
       (fvsB, (bsB, eB)) <- expE aiB vmB eBody
       return (fvsL `S.union` (fvsB S.\\ S.singleton vIx),
               (bsL ++ shiftBIs bsB,
                App op t [eL, AIn r1 $ Lambda vIx eB]))
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

simpleExp ai vm e = simpleArrRef e || expCost ai vm e <= 2

expCost :: [AbsInfo] -> VarMap -> UExp -> Int
expCost ai vm (AIn _ e) = go e
  where go (Variable _)  = 0
        go (Literal _)   = 0
        go (App op _ es) = appCost ai vm op es
        go _             = 5

appCost :: [AbsInfo] -> VarMap -> Op -> [UExp] -> Int
appCost ai vm = go
  where go Mul [AIn _ (Variable v), AIn _ (Variable u)]
           | ixAndInv ai v (fst $ vm M.! u) = 1
        go _   _                            = 5

ixAndInv :: [AbsInfo] -> Var -> S.Set Var -> Bool
ixAndInv ai v vs = go ai
  where go (LoopI{ixVar = ix, absVars = avs} : ai) = disjoint vvs avs && (ix == v || go ai)
        go (AbsI{absVars = avs} : ai) = disjoint vvs avs && go ai
        go [] = True
        vvs = S.union vs $ S.singleton v

-- | Check that an expression is a simple array reference that we can inline
simpleArrRef :: UExp -> Bool
simpleArrRef (AIn _ (App GetIx _ [AIn _ (Variable _), e])) = simpleIdxE e
  where simpleIdxE (AIn _ (Variable _)) = True
        simpleIdxE (AIn _ (App op _ es)) = elem op [Add,Sub,Mul] && all simpleIdxE es
        simpleIdxE _ = False
simpleArrRef _ = False

disjoint :: Ord a => S.Set a -> S.Set a -> Bool
disjoint xs ys = S.null $ S.intersection xs ys
