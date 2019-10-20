{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Core.UntypedRepresentation (
    VarId (..)
  , Term(..)
  , UntypedFeld
  , mkLam'
  , ATerm(..)
  , AUntypedFeld
  , UntypedFeldF(..)
  , Op(..)
  , Type(..)
  , Lit(..)
  , Var(..)
  , Size(..)
  , Signedness(..)
  , Fork(..)
  , HasType(..)
  , unAnnotate
  , annotate
  , getAnnotation
  , dropAnnotation
  , fvA
  , fv
  , allVars
  , collectLetBinders
  , collectBinders
  , mkLets
  , mkLam
  , mkApp
  , mkTup
  , subst
  , stringTree
  , stringTreeExp
  , prettyExp
  , prettyVI
  , aLit
  , topInfo
  , botInfo
  , indexType
  , elementsVI
  , sharable
  , legalToShare
  , goodToShare
  , Rename(..)
  , rename
  , newVar
  , simpleInline
  )
  where

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Data.List (nub, intercalate)
import Data.Tree
import Data.Int
import Data.Word

import Language.Syntactic.Constructs.Binding (VarId (..))

import Feldspar.Range (Range(..), singletonRange, fullRange, emptyRange)
import Feldspar.Core.Types (Length)
import Feldspar.ValueInfo (ValueInfo(..), singletonVI, lubVI, boolBot, boolTop)

-- This file contains the UntypedFeld format and associated
-- helper-formats and -functions that work on those formats, for
-- example fv and typeof.
--
-- The format resembles the structure of the typed Syntactic format,
-- but it does not reflect into the host language type system.

type UntypedFeld = Term UntypedFeldF

-- | Inverse of collectBinders, make a lambda abstraction.
mkLam' :: [(a,Var)] -> AUntypedFeld a -> AUntypedFeld a
mkLam' []        e = e
mkLam' ((a,h):t) e = AIn a (Lambda h (mkLam' t e))

data Term f = In (f (Term f))

deriving instance (Eq (f (Term f))) => Eq (Term f)
instance (Show (f (Term f))) => Show (Term f) where
  show (In f) = show f

-- | Types representing an annotated term
type AUntypedFeld a = ATerm a UntypedFeldF

data ATerm a f = AIn a (f (ATerm a f))

deriving instance (Eq a, Eq (f (ATerm a f))) => Eq (ATerm a f)
instance (Show (f (ATerm a f))) => Show (ATerm a f) where
  show (AIn _ f) = show f

-- | Remove annotations and translate to UntypedFeld
unAnnotate :: AUntypedFeld a -> UntypedFeld
unAnnotate (AIn _ e) = In $ go e
  where go (Lambda v e)         = Lambda v (unAnnotate e)
        go (LetFun (s,k,e1) e2) = LetFun (s, k, unAnnotate e1) (unAnnotate e2)
        go (App f t es)         = App f t (map unAnnotate es)
        go (Variable v)         = Variable v
        go (Literal l)          = Literal l

-- | Add annotations using an annotation function
annotate :: (UntypedFeldF (AUntypedFeld a) -> a) -> UntypedFeld -> AUntypedFeld a
annotate anno e = goA e
  where go (Lambda v e)         = Lambda v $ goA e
        go (LetFun (s,k,e1) e2) = LetFun (s, k, goA e1) $ goA e2
        go (App f t es)         = App f t $ map goA es
        go (Variable v)         = Variable v
        go (Literal l)          = Literal l
        goA (In e)              = let e1 = go e in AIn (anno e1) e1

-- | Extract the annotation part of an AUntypedFeld
getAnnotation :: AUntypedFeld a -> a
getAnnotation (AIn r _) = r

-- | Drop the annotation part of an AUntypedFeld
dropAnnotation :: AUntypedFeld a -> UntypedFeldF (AUntypedFeld a)
dropAnnotation (AIn _ e) = e

data Size = S8 | S16 | S32 | S40 | S64
          | S128 -- Used by SICS.
    deriving (Eq,Show,Enum,Ord)

data Signedness = Signed | Unsigned
    deriving (Eq,Show,Enum)

data Fork = None | Future | Par | Loop
    deriving (Eq,Show)

data Type =
     BoolType
   | BitType
   | IntType Signedness Size
   | FloatType
   | DoubleType
   | ComplexType Type
   | TupType [Type]
   | MutType Type
   | RefType Type
   | ArrayType (Range Length) Type
   | MArrType (Range Length) Type
   | ParType Type
   | ElementsType Type
   | IVarType Type
   | FunType Type Type
   | FValType Type
   deriving (Eq,Show)


data Var = Var { varNum :: VarId
               , varType :: Type
               , varName :: B.ByteString
               }

-- Variables are equal if they have the same varNum.
instance Eq Var where
  v1 == v2 = varNum v1 == varNum v2

instance Ord Var where
  compare v1 v2 = compare (varNum v1) (varNum v2)

instance Show Var where
  show (Var n _t name) = (if name == B.empty
                            then "v"
                            else B.unpack name) ++ show n

data Lit =
     LBool Bool
   | LInt Signedness Size Integer
   | LFloat Float
   | LDouble Double
   | LComplex Lit Lit
   | LArray Type [Lit] -- Type necessary for empty array literals.
   | LTup [Lit]
   deriving (Eq)

-- | Make value info from a literal
literalVI :: Lit -> ValueInfo
literalVI (LBool b) = singletonVI b
literalVI (LInt sgn sz n) = go sgn sz
  where go Signed     S8 = singletonVI (fromInteger n :: Int8)
        go Signed    S16 = singletonVI (fromInteger n :: Int16)
        go Signed    S32 = singletonVI (fromInteger n :: Int32)
        go Signed    S40 = singletonVI (fromInteger n :: Int64)
        go Signed    S64 = singletonVI (fromInteger n :: Int64)
        go Signed   S128 = error $ "UntypedRepresentation.literalVI: not supported"
        go Unsigned   S8 = singletonVI (fromInteger n :: Word8)
        go Unsigned  S16 = singletonVI (fromInteger n :: Word16)
        go Unsigned  S32 = singletonVI (fromInteger n :: Word32)
        go Unsigned  S40 = singletonVI (fromInteger n :: Word64)
        go Unsigned  S64 = singletonVI (fromInteger n :: Word64)
        go Unsigned S128 = error $ "UntypedRepresentation.literalVI: not supported"
literalVI (LFloat  x) = singletonVI x
literalVI (LDouble x) = singletonVI x
literalVI (LComplex re im) = VIProd [literalVI re, literalVI im]
literalVI (LArray t xs) = foldr lubVI (botInfo t) $ map literalVI xs
literalVI (LTup xs) = VIProd $ map literalVI xs

-- | The bottom (most informative) elements of the info domains for each type.
botInfo :: Type -> ValueInfo
botInfo BoolType         = boolBot
botInfo BitType          = VIWord8 $ Range 1 0 -- Provisionally
botInfo (IntType sgn sz) = constantIntRange sgn sz emptyRange
botInfo FloatType        = VIFloat
botInfo DoubleType       = VIDouble
botInfo (ComplexType t)  = VIProd [botInfo t, botInfo t]
botInfo (TupType ts)     = VIProd $ map botInfo ts
botInfo (MutType t)      = botInfo t
botInfo (RefType t)      = botInfo t
botInfo (ArrayType r t)  = VIProd [VIWordN r, botInfo t]
botInfo (MArrType r t)   = VIProd [VIWordN r, botInfo t]
botInfo (ParType t)      = botInfo t -- Provisionally
botInfo (ElementsType t) = VIProd [botInfo indexType, botInfo t]
botInfo (IVarType t)     = botInfo t
botInfo (FunType _ t)    = botInfo t
botInfo (FValType t)     = botInfo t

-- | The top (least informative) elements of the info domains for each type.
topInfo :: Type -> ValueInfo
topInfo BoolType         = boolTop
topInfo BitType          = VIWord8 $ Range 0 1 -- Provisionally
topInfo (IntType sgn sz) = constantIntRange sgn sz fullRange
topInfo FloatType        = VIFloat
topInfo DoubleType       = VIDouble
topInfo (ComplexType t)  = VIProd [topInfo t, topInfo t]
topInfo (TupType ts)     = VIProd $ map topInfo ts
topInfo (MutType t)      = topInfo t
topInfo (RefType t)      = topInfo t
topInfo (ArrayType r t)  = VIProd [VIWordN r, topInfo t]
topInfo (MArrType r t)   = VIProd [VIWordN r, topInfo t]
topInfo (ParType t)      = topInfo t -- Provisionally
topInfo (ElementsType t) = VIProd [topInfo indexType, topInfo t]
topInfo (IVarType t)     = topInfo t
topInfo (FunType _ t)    = topInfo t
topInfo (FValType t)     = topInfo t

-- | Pretty printing a value info given a type
prettyVI :: Type -> ValueInfo -> String
prettyVI t (VIBool r)   = show r
prettyVI t (VIInt8 r)   = show r
prettyVI t (VIInt16 r)  = show r
prettyVI t (VIInt32 r)  = show r
prettyVI t (VIInt64 r)  = show r
prettyVI t (VIIntN r)   = show r
prettyVI t (VIWord8 r)  = show r
prettyVI t (VIWord16 r) = show r
prettyVI t (VIWord32 r) = show r
prettyVI t (VIWord64 r) = show r
prettyVI t (VIWordN r)  = show r
prettyVI t (VIFloat)    = "[*,*]"
prettyVI t (VIDouble)   = "[*,*]"
prettyVI t (VIProd vs)  = pr t vs
  where pr (ArrayType _ t)  [v1,v2] = prettyVI indexType v1 ++ " :> " ++ prettyVI t v2
        pr (ElementsType t) [v1,v2] = prettyVI indexType v1 ++ " :>> " ++ prettyVI t v2
        pr (TupType ts)     vs      = "(" ++ intercalate ", " (zipWith prettyVI ts vs) ++ ")"
        pr t                vs      = "VIProd " ++ show vs

-- | The Type used to represent indexes, to which Index is mapped.
indexType :: Type
indexType = IntType Unsigned S32

-- | Construct an Elements value info from those of the index and value parts
elementsVI :: ValueInfo -> ValueInfo -> ValueInfo
elementsVI idx val = VIProd [idx, val]

-- | Forcing a range to an integer type given by a signedness and a size.
constantIntRange :: Signedness -> Size -> (forall a . Bounded a => Range a)
                    -> ValueInfo
constantIntRange Signed   S8  r = VIInt8 r
constantIntRange Signed   S16 r = VIInt16 r
constantIntRange Signed   S32 r = VIInt32 r
constantIntRange Signed   S64 r = VIInt64 r
constantIntRange Unsigned S8  r = VIWord8 r
constantIntRange Unsigned S16 r = VIWord16 r
constantIntRange Unsigned S32 r = VIWord32 r
constantIntRange Unsigned S64 r = VIWord64 r

-- | Human readable show instance.
instance Show Lit where
   show (LBool b)                    = show b
   show (LInt _ _ i)                 = show i
   show (LFloat f)                   = show f
   show (LDouble d)                  = show d
   show (LComplex r c)               = "(" ++ show r ++ ", " ++ show c ++ "i)"
   show (LArray _ ls)                = "[" ++ sls ++ "]"
     where sls = intercalate "," $ map show ls
   show (LTup ls)                    = "(" ++ intercalate ", " (map show ls) ++ ")"

-- | Application heads.
data Op =
   -- Array
     GetLength
   | Parallel
   | Append
   | GetIx
   | SetLength
   | Sequential
   | SetIx
   -- Binding
   | Let
   -- Bits
   | Bit
   | Complement
   | ReverseBits
   | BitScan
   | BitCount
   | BAnd
   | BOr
   | BXor
   | SetBit
   | ClearBit
   | ComplementBit
   | TestBit
   | ShiftLU
   | ShiftRU
   | ShiftL
   | ShiftR
   | RotateLU
   | RotateRU
   | RotateL
   | RotateR
   -- Complex
   | RealPart
   | ImagPart
   | Conjugate
   | Magnitude
   | Phase
   | Cis
   | MkComplex
   | MkPolar
   -- Condition
   | Condition
   | ConditionM
   -- Conversion
   | F2I
   | I2N
   | B2I
   | Round
   | Ceiling
   | Floor
   -- Elements
   | ESkip
   | EMaterialize
   | EWrite
   | EPar
   | EparFor
   -- Eq
   | Equal
   | NotEqual
   -- Error
   | Undefined
   | Assert String
   -- FFI
   | ForeignImport String
   -- Floating
   | Exp
   | Sqrt
   | Log
   | Sin
   | Tan
   | Cos
   | Asin
   | Atan
   | Acos
   | Sinh
   | Tanh
   | Cosh
   | Asinh
   | Atanh
   | Acosh
   | Pow
   | LogBase
   -- Floating
   | Pi
   -- Fractional
   | DivFrac
   -- Future
   | MkFuture
   | Await
   -- Integral
   | Quot
   | Rem
   | Div
   | Mod
   | IExp
   -- Logic
   | Not
   -- Logic
   | And
   | Or
   -- Loop
   | ForLoop
   | WhileLoop
   -- LoopM
   | While
   | For
   -- Mutable
   | Run
   | Return
   | Bind
   | Then
   | When
   -- MutableArray
   | NewArr_
   | ArrLength
   | NewArr
   | GetArr
   | SetArr
   -- MutableToPure
   | RunMutableArray
   | WithArray
   -- MutableReference
   | NewRef
   | GetRef
   | SetRef
   | ModRef
   -- Noinline
   | NoInline
   -- Num
   | Abs
   | Sign
   | Add
   | Sub
   | Mul
   -- Par
   | ParRun
   | ParGet
   | ParFork
   | ParNew
   | ParYield
   | ParPut
   -- Ord
   | LTH
   | GTH
   | LTE
   | GTE
   | Min
   | Max
   -- RealFloat
   | Atan2
   -- Save
   | Save
   -- SizeProp
   | PropSize
   -- SourceInfo
   | SourceInfo String
   -- Switch
   | Switch
   -- Tuples
   | Tup
   | Sel1
   | Sel2
   | Sel3
   | Sel4
   | Sel5
   | Sel6
   | Sel7
   | Sel8
   | Sel9
   | Sel10
   | Sel11
   | Sel12
   | Sel13
   | Sel14
   | Sel15
   -- Common nodes
   | Call Fork String
   deriving (Eq, Show)

-- | The main type: Variables, Bindings, Literals and Applications.
data UntypedFeldF e =
   -- Binding
     Variable Var
   | Lambda Var e
   | LetFun (String, Fork, e) e -- Note [Function bindings]
   -- Literal
   | Literal Lit
   -- Common nodes
   | App Op Type [e]
   deriving (Eq)

{-

Function bindings
-----------------

The LetFun constructor is different from the ordinary let-bindings,
and therefore has its own node type. In an ordinary language the
constructor would be called LetRec, but we do not have any
recursion. Functions are created by the createTasks pass, and they can
be run sequentially or concurrently depending on the "Fork".

-}

instance (Show e) => Show (UntypedFeldF e) where
   show (Variable v)                = show v
   show (Lambda v e)                = "(\\" ++ show v ++ " -> " ++ show e ++ ")"
   show (LetFun (s, k, e1) e2)      = "letFun " ++ show k ++ " " ++ s ++" = "++ show e1 ++ " in " ++ show e2
   show (Literal l) = show l
   show (App p@RunMutableArray _ [e]) = show p ++ " (" ++ show e ++ ")"
   show (App GetIx _ [e1,e2])       = "(" ++ show e1 ++ " ! " ++ show e2 ++ ")"
   show (App Add _ [e1,e2])         = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
   show (App Sub _ [e1,e2])         = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
   show (App Mul _ [e1,e2])         = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
   show (App Div _ [e1,e2])         = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
   show (App p@Then _ [e1, e2])     = show p ++ " (" ++ show e1 ++ ") (" ++
                                      show e2 ++ ")"
   show (App p _ [e1, e2])
    | p `elem` [Bind, Let, EPar]    = show p ++ " (" ++ show e1 ++ ") " ++ show e2
   show (App (ForeignImport s) _ es)= s ++ " " ++ unwords (map show es)
   show (App Tup _ es)              = "("   ++ intercalate ", " (map show es) ++ ")"
   show (App p@Parallel _ [e1,e2]) = show p ++ " (" ++ show e1 ++ ") " ++ show e2
   show (App p@Sequential _ [e1,e2,e3]) = show p ++ " (" ++ show e1 ++ ") (" ++ show e2 ++ ") " ++ show e3
   show (App p t es)
    | p `elem` [F2I, I2N, B2I, Round, Ceiling, Floor]
    = show p ++ "{" ++ show t ++ "} " ++ unwords (map show es)
   show (App p _ es)                = show p ++ " " ++ unwords (map show es)

-- | Compute a compact text representation of a type
prType :: Type -> String
prType BoolType         = "bool"
prType BitType          = "bit"
prType (IntType s sz)   = prS s ++ prSz sz
  where prS Signed   = "i"
        prS Unsigned = "u"
        prSz s       = drop 1 $ show s
prType FloatType        = "f32"
prType DoubleType       = "f64"
prType (ComplexType t)  = "c" ++ prType t
prType (TupType ts)     = "(" ++ intercalate "," (map prType ts) ++ ")"
prType (MutType t)      = "M" ++ prType t
prType (RefType t)      = "R" ++ prType t
prType (ArrayType _ t)  = "a" ++ prType t
prType (MArrType _ t)   = "A" ++ prType t
prType (ParType t)      = "P" ++ prType t
prType (ElementsType t) = "e" ++ prType t
prType (IVarType t)     = "V" ++ prType t
prType (FunType t1 t2)  = "(" ++ prType t1 ++ "->" ++ prType t2 ++ ")"
prType (FValType t)     = "F" ++ prType t

-- | Convert an untyped unannotated syntax tree into a @Tree@ of @String@s
stringTree :: UntypedFeld -> Tree String
stringTree = stringTreeExp (const "") . annotate (const ())

-- | Convert an untyped annotated syntax tree into a @Tree@ of @String@s
stringTreeExp :: (a -> String) -> AUntypedFeld a -> Tree String
stringTreeExp prA = go
  where
    go (AIn r (Variable v))         = Node (show v ++ prC (typeof v) ++ prA r) []
    go (AIn _ (Lambda v e))         = Node ("Lambda "++show v ++ prC (typeof v)) [go e]
    go (AIn _ (LetFun (s,k,e1) e2)) = Node (unwords ["LetFun", show k, s]) [go e1, go e2]
    go (AIn _ (Literal l))          = Node (show l ++ prC (typeof l)) []
    go (AIn r (App Let t es))       = Node "Let" $ goLet $ AIn r (App Let t es)
    go (AIn r (App p t es))         = Node (show p ++ prP t r) (map go es)
    goLet (AIn _ (App Let _ [eRhs, AIn _ (Lambda v e)]))
                                    = Node ("Var " ++ show v ++ prC (typeof v) ++ " = ") [go eRhs]
                                    : goLet e
    goLet e = [Node "In" [go e]]
    prP t r = " {" ++ prType t ++ prA r ++ "}"
    prC t   = " : " ++ prType t

prettyExp :: (Type -> a -> String) -> AUntypedFeld a -> String
prettyExp prA e = render (pr 0 0 e)
  where pr p i (AIn r e) = pe p i r e
        pe p i r (Variable v) = line i $ show v
        pe p i _ (Literal l) = line i $ show l
        pe p i _ (Lambda v e) = par p 0 $ join $ (line i $ "\\ " ++ pv Nothing v ++ " ->") ++ pr 0 (i+2) e
        pe p i r (App Let t es) = par p 0 $ line i "let" ++ pLet i (AIn r $ App Let t es)
        pe p i r (App f t es) = par p 10 $ join $ (line i $ show f ++ prP t r) ++ pArgs p (i+2) es

        pArgs p i [e@(AIn _ (Lambda _ _))] = pr p i e
        pArgs p i (e:es) = pr 11 i e ++ pArgs p i es
        pArgs p i [] = []

        pLet i (AIn _ (App Let _ [eRhs, AIn _ (Lambda v e)]))
               = join (line (i+2) (pv Nothing v ++ " =") ++ pr 0 (i+4) eRhs) ++ pLet i e
        pLet i e = line i "in" ++ pr 0 (i+2) e

        pv mr v = show v ++ prC (typeof v) ++ maybe "" (prA $ typeof v) mr

        prP t r = " {" ++ prType t ++ prA t r ++ "}"
        prC t   = " : " ++ prType t

        par p i [] = error $ "UntypedRepresentation.prettyExp: parethesisizing empty text"
        par p i ls = if p <= i then ls else prepend "(" $ append ")" ls
        prepend s ((i,n,v) : ls) = ((i, n + length s, s ++ v) : ls)
        append s [(i,n,v)] = [(i, n + length s, v ++ s)]
        append s (l:ls) = l : append s ls

        join (x:y:xs)
          | indent x <= indent y &&
            all ((==) (indent y) . indent) xs &&
            l <= 60
          = [(indent x, l, intercalate " " $ map val $ x:y:xs)]
            where l = sum (map len $ x:y:xs) + length xs + 1
        join xs = xs
        render ls = foldr (\ (i,_,cs) str -> replicate i ' ' ++ cs ++ "\n" ++ str) "" ls
        line i cs = [(i, length cs, cs)]
        indent (i,_,_) = i
        len (_,l,_) = l
        val (_,_,v) = v

        -- In the precedence argument, 0 means that no expressions need parentesis,
        -- wheras 10 accepts applications and 11 only accepts atoms (variables and literals)

class HasType a where
    type TypeOf a
    typeof :: a -> TypeOf a

instance HasType Var where
    type TypeOf Var = Type
    typeof Var{..}  = varType

instance HasType Lit where
    type TypeOf Lit      = Type
    typeof (LInt s n _)  = IntType s n
    typeof LDouble{}     = DoubleType
    typeof LFloat{}      = FloatType
    typeof LBool{}       = BoolType
    typeof (LArray t es) = ArrayType (singletonRange $ fromIntegral $ length es) t
    typeof (LComplex r _) = ComplexType $ typeof r
    typeof (LTup ls)     = TupType (map typeof ls)

instance HasType UntypedFeld where
    type TypeOf UntypedFeld                = Type
   -- Binding
    typeof (In (Variable v))               = typeof v
    typeof (In (Lambda v e))               = FunType (typeof v) (typeof e)
    typeof (In (LetFun _ e))               = typeof e
   -- Literal
    typeof (In (Literal l))                = typeof l
    typeof (In (App _ t _))                = t

instance HasType (AUntypedFeld a) where
    type TypeOf (AUntypedFeld a)           = Type
   -- Binding
    typeof (AIn _ (Variable v))            = typeof v
    typeof (AIn _ (Lambda v e))            = FunType (typeof v) (typeof e)
    typeof (AIn _ (LetFun _ e))            = typeof e
   -- Literal
    typeof (AIn _ (Literal l))             = typeof l
    typeof (AIn _ (App _ t _))             = t

fvA :: AUntypedFeld a -> [Var]
fvA e = fv $ unAnnotate e

fv :: UntypedFeld -> [Var]
fv = nub . fvU' []

fvU' :: [Var] -> UntypedFeld -> [Var]
   -- Binding
fvU' vs (In (Variable v)) | v `elem` vs  = []
                          | otherwise    = [v]
fvU' vs (In (Lambda v e))                = fvU' (v:vs) e
fvU' vs (In (LetFun (_, _, e1) e2))      = fvU' vs e1 ++ fvU' vs e2
   -- Literal
fvU' _  (In (Literal{}))                 = []
-- Common nodes.
fvU' vs (In (App _ _ es))                = concatMap (fvU' vs) es

-- | List all variables (free, bound and introduced by lambdas) in an expression
allVars :: UntypedFeld -> [Var]
allVars = nub . go
  where
    go (In (Variable v))           = [v]
    go (In (Lambda v e))           = v : go e
    go (In (LetFun (_, _, e1) e2)) = go e1 ++ go e2
    go (In (Literal{}))            = []
    go (In (App _ _ es))           = concatMap go es

-- | Collect nested let binders into the binders and the body.
collectLetBinders :: AUntypedFeld a -> ([(Var, AUntypedFeld a)], AUntypedFeld a)
collectLetBinders = go []
  where go acc (AIn _ (App Let _ [e, AIn _ (Lambda v b)])) = go ((v, e):acc) b
        go acc e                                           = (reverse acc, e)

-- | Collect binders from nested lambda expressions.
collectBinders :: AUntypedFeld a -> ([(a, Var)], AUntypedFeld a)
collectBinders = go []
  where go acc (AIn a (Lambda v e)) = go ((a,v):acc) e
        go acc e                    = (reverse acc, e)

-- | Inverse of collectLetBinders, put the term back together.
mkLets :: ([(Var, AUntypedFeld a)], AUntypedFeld a) -> AUntypedFeld a
mkLets ([], body)       = body
mkLets ((v, e):t, body) = AIn r (App Let t' [e, body'])
  where body'        = AIn r (Lambda v (mkLets (t, body))) -- Value info of result
        FunType _ t' = typeof body'
        r            = getAnnotation body

-- | Inverse of collectBinders, make a lambda abstraction.
mkLam :: [Var] -> UntypedFeld -> UntypedFeld
mkLam []    e = e
mkLam (h:t) e = In (Lambda h (mkLam t e))

-- | Make an application.
mkApp :: Type -> Op -> [UntypedFeld] -> UntypedFeld
mkApp t p es = In (App p t es)

-- | Make a tuple; constructs the type from the types of the components
mkTup :: a -> [AUntypedFeld a] -> AUntypedFeld a
mkTup a es = AIn a $ App Tup (TupType $ map typeof es) es

-- | Substitute new for dst in e. Assumes no shadowing.
subst :: AUntypedFeld a -> Var -> AUntypedFeld a -> AUntypedFeld a
subst new dst = go
  where go v@(AIn _ (Variable v')) | dst == v' = new -- Replace.
                                   | otherwise = v -- Stop.
        go l@(AIn r (Lambda v e')) | v == dst  = l -- Stop.
                                   | otherwise = AIn r (Lambda v (go e'))
        go (AIn r (LetFun (s, k, e1) e2))
           = AIn r (LetFun (s, k, go e1) (go e2)) -- Recurse.
        go l@(AIn _ Literal{})  = l -- Stop.
        go (AIn r (App p t es)) = AIn r (App p t (map go es)) -- Recurse.

-- | Annotate a literal with value info.
aLit :: Lit -> AUntypedFeld ValueInfo
aLit l = AIn (literalVI l) (Literal l)

-- | Expressions that can and should be shared
sharable :: AUntypedFeld a -> Bool
sharable e = legalToShare e && goodToShare e

-- | Expressions that can be shared without breaking fromCore
legalToShare :: AUntypedFeld a -> Bool
legalToShare (AIn _ (App op _ _)) = op `notElem` [ESkip, EWrite, EPar, EparFor,
                                                  Return, Bind, Then, When,
                                                  NewArr, NewArr_, GetArr, SetArr, ArrLength,
                                                  For, While,
                                                  NewRef, GetRef, SetRef, ModRef]
legalToShare (AIn _ (Lambda _ _)) = False
legalToShare _                    = True

-- | Expressions that are expensive enough to be worth sharing
goodToShare :: AUntypedFeld a -> Bool
goodToShare (AIn _ (Literal l)) 
  | LArray _ (_:_) <- l = True
  | LTup (_:_)     <- l = True
goodToShare (AIn _ (App _ _ _)) = True
goodToShare _                   = False

type Rename a = State VarId a

rename :: AUntypedFeld a -> Rename (AUntypedFeld a)
rename = renameA M.empty

type RRExp a = UntypedFeldF (ATerm a UntypedFeldF)

renameA :: M.Map VarId (RRExp a) -> AUntypedFeld a -> Rename (AUntypedFeld a)
renameA env (AIn a r) = do r1 <- renameR env r
                           return $ AIn a r1

renameR :: M.Map VarId (RRExp a) -> RRExp a -> Rename (RRExp a)
renameR env (Variable v) = return $ env M.! varNum v
renameR env (App f t es) = do es1 <- mapM (renameA env) es
                              return $ App f t es1
renameR env (Lambda v e) = do v1 <- newVar v
                              e1 <- renameA (M.insert (varNum v) (Variable v1) env) e
                              return $ Lambda v1 e1
renameR env (Literal l) = return $ Literal l
renameR env e = error $ "FromTyped.renameR: unexpected expression " ++ show e

newVar v = do j <- get
              put (j+1)
              return $ v{varNum = j}

-- | Inline everything that is not sharable
simpleInline :: AUntypedFeld a -> AUntypedFeld a
simpleInline = goA M.empty
  where goA m (AIn a r) = AIn a $ go m r
        go m (Variable v) = M.findWithDefault (Variable v) (varNum v) m
        go m (Literal l) = Literal l
        go m (App Let _ [rhs, AIn a (Lambda v e)]) | not $ sharable rhs
             = unA $ goA (M.insert (varNum v) (unA $ goA m rhs) m) e
        go m (App op t es) = App op t $ map (goA m) es
        go m (Lambda v e) = Lambda v (goA m e) -- Here we assume no name capture or shadowing
        unA (AIn _ r) = r


