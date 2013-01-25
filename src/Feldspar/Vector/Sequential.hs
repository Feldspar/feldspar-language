module Feldspar.Vector.Sequential where

import Feldspar

data Seq a where
  Seq :: Syntactic s => Data Length -> s -> (Data Index -> s -> (a,s)) -> Seq a

length :: Seq a -> Data Length
length (Seq l _ _) = l

toArray :: Seq (Data a) -> Data [a]
toArray (Seq l init next) = sequential l init next

instance Functor Seq where
  fmap f (Seq l i next) = Seq l i (\i s -> let (a,s') = next i s
                                           in (f a,s'))

seq :: Syntactic s => Data Length -> s -> (Data Index -> s -> (a,s)) -> Seq s
seq l i next = Seq l i next


-- Return a sequence of numbers between -0.5 and 0.5 to indicate 
demod :: Data Float -> Data WordN -> Seq (Data Float)
demod c i = seq i c next
  where next _ c = let a = clamp c
                   in (a, if a >= 0 then (a - 0.5) * 2 else (a + 0.5) * 2)

interleave :: Seq a -> Seq a -> Seq a
interleave (Seq l1 i1 n1) (Seq l2 i2 n2)
  = Seq (l1+l2) (true,i1,0,i2,0) next
  where next i (b,s1,i1,s2,i2) = b ? (let (a,s1') = n1 i1 s1
  	       		       	      in (a,(false,s1',i1+1,s2,i2))
				     ,let (a,s2') = n2 i2 s2
				      in (a,(true,s1,i1,s2',i2+1)))

qam64 :: Data (Complex Float) -> Seq (Data Float)
qam64 c = interleave (demod (real c)) (demod (imag c))
