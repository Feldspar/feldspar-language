module Feldspar.Core.Middleend.CreateTasks ( createTasks ) where

import Control.Monad.State

import Feldspar.Core.UntypedRepresentation

-- | Create tasks from MkFuture and similar constructs.
createTasks :: UntypedFeld -> UntypedFeld
createTasks e = evalState (go e) 0

go :: UntypedFeld -> State Integer UntypedFeld
go e@(In Variable{}) = return e
go (In (Lambda v e)) = do
  e' <- go e
  return $ In (Lambda v e')
go (In (LetFun (s, k, e1) e2))
 = liftM2 (\e1' e2' -> In (LetFun (s, k, e1') e2')) (go e1) (go e2)
go l@(In Literal{}) = return l
go (In (App MkFuture _ [p])) = do
  p'' <- go p'
  i <- freshId
  let taskName = "task" ++ show i
      core = "task_core" ++ show i
  return $ In (LetFun (core, Future, p'') (In (App (Call Future taskName) t' vs')))
   where vs = fv p
         vs' = map (In . Variable) vs
         p' = mkLam vs p
         t' = typeof p'
go (In (App NoInline _ [p])) = do
  p'' <- go p'
  i <- freshId
  let name = "noinline" ++ show i
  return $ In (LetFun (name, None, p'') (In (App (Call None name) t' vs')))
   where vs = fv p
         vs' = map (In . Variable) vs
         p' = mkLam vs p
         t' = typeof p'
go (In (App p t es)) = do
  es' <- mapM go es
  return $ In (App p t es')

freshId :: State Integer Integer
freshId = do
   i <- get
   put (i + 1)
   return i
