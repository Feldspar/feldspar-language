module Feldspar.Core.Middleend.PassManager where

data Prog a b = Prog (Maybe a) [String] b
  deriving Show

data PassCtrl a = PassCtrl
       { wrBefore   :: [a]
       , wrAfter    :: [a]
       , stopBefore :: [a]
       , stopAfter  :: [a]
       , skip       :: [a]
       }
  deriving Show

addWrBefore :: PassCtrl a -> a -> PassCtrl a
addWrBefore ctrl p = ctrl{wrBefore = p : wrBefore ctrl}

addWrAfter :: PassCtrl a -> a -> PassCtrl a
addWrAfter ctrl p = ctrl{wrAfter = p : wrAfter ctrl}

setStopBefore :: PassCtrl a -> a -> PassCtrl a
setStopBefore ctrl p = ctrl{stopBefore = [p]}

setStopAfter :: PassCtrl a -> a -> PassCtrl a
setStopAfter ctrl p = ctrl{stopAfter = [p]}

addSkip :: PassCtrl a -> a -> PassCtrl a
addSkip ctrl p = ctrl{skip = p : skip ctrl}

defaultPassCtrl :: PassCtrl a
defaultPassCtrl = PassCtrl [] [] [] [] []

class Pretty a where
  pretty :: a -> String

prOrStop :: (Pretty a, Eq b, Show b) => String -> [b] -> [b] -> b -> Prog a c -> Prog a c
prOrStop pos prs stop pass (Prog (Just p) ss s)
  = Prog (if elem pass stop then Nothing else Just p)
         (ss ++ if elem pass prs then [preamble ++ pretty p ++ "\n"] else [])
         s
  where preamble = "\n========== " ++ pos ++ " " ++ show pass ++ " ==========\n\n"
prOrStop _ _ _ _ prog = prog

runPassC :: (Eq b ) => [b] -> b -> (a -> a) -> Prog a c -> Prog a c
runPassC skips pass f (Prog (Just p) ss s)
  = Prog (Just $ if elem pass skips then p else f p) ss s
runPassC _ _ _ prog = prog

runPassT :: (a -> b) -> Prog a c -> Prog b c
runPassT f (Prog (Just p) ss s) = Prog (Just $ f p) ss s
runPassT f (Prog Nothing ss s) = Prog Nothing ss s

runPassS :: Eq b => [b] -> b -> ((c,a) -> (c,a)) -> Prog a c -> Prog a c
runPassS skips pass f (Prog (Just p) ss s)
  | notElem pass skips = Prog (Just p1) ss s1
  where (s1,p1) = f (s,p)

passC :: (Pretty a, Eq b, Show b) => PassCtrl b -> b -> (a -> a) -> Prog a c -> Prog a c
passC ctrl pass f = prOrStop "After" (wrAfter ctrl) (stopAfter ctrl) pass
                  . runPassC (skip ctrl) pass f
                  . prOrStop "Before" (wrBefore ctrl) (stopBefore ctrl) pass

passT :: (Pretty a, Pretty d, Eq b, Show b) => PassCtrl b -> b -> (a -> d) -> Prog a c -> Prog d c
passT ctrl pass f = prOrStop "After" (wrAfter ctrl) (stopAfter ctrl) pass
                  . runPassT f
                  . prOrStop "Before" (wrBefore ctrl) (stopBefore ctrl) pass

passS :: (Pretty a, Eq b, Show b) => PassCtrl b -> b -> ((c,a) -> (c,a)) -> Prog a c -> Prog a c
passS ctrl pass f = prOrStop "After" (wrAfter ctrl) (stopAfter ctrl) pass
                  . runPassS (skip ctrl) pass f
                  . prOrStop "Before" (wrBefore ctrl) (stopBefore ctrl) pass

evalPasses :: c -> (Prog a c -> Prog b c) -> a -> ([String], Maybe b)
evalPasses s f p = case f $ Prog (Just p) [] s of
                     Prog prg ss _ -> (ss, prg)

