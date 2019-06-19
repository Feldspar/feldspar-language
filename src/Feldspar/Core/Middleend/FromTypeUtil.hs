{-# LANGUAGE GADTs #-}
module Feldspar.Core.Middleend.FromTypeUtil where

import Data.Complex

import Language.Syntactic
import Feldspar.Core.Types
import Feldspar.Core.UntypedRepresentation hiding ( Lambda, UntypedFeldF(..)
                                                  , Size, Type(..), Signedness
                                                  , Op(..)
                                                  )
import qualified Feldspar.Core.UntypedRepresentation as Ut
import Feldspar.ValueInfo
import Feldspar.Range

untypeType :: TypeRep a -> Size a -> Ut.Type
untypeType UnitType _               = Ut.TupType []
untypeType BoolType _               = Ut.BoolType
untypeType (IntType s n) _          = Ut.IntType (convSign s) (convSize n)
untypeType FloatType _              = Ut.FloatType
untypeType DoubleType _             = Ut.DoubleType
untypeType (ComplexType t) _        = Ut.ComplexType (untypeType t (defaultSize t))
untypeType (Tup2Type a b) (sa,sb)
  = Ut.TupType [untypeType a sa, untypeType b sb]
untypeType (Tup3Type a b c) (sa,sb,sc)
  = Ut.TupType [untypeType a sa, untypeType b sb, untypeType c sc]
untypeType (Tup4Type a b c d) (sa,sb,sc,sd)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd
               ]
untypeType (Tup5Type a b c d e) (sa,sb,sc,sd,se)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se
               ]
untypeType (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               ]
untypeType (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg
               ]
untypeType (Tup8Type a b c d e f g h) (sa,sb,sc,sd,se,sf,sg,sh)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh
               ]
untypeType (Tup9Type a b c d e f g h i) (sa,sb,sc,sd,se,sf,sg,sh,si)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               ]
untypeType (Tup10Type a b c d e f g h i j) (sa,sb,sc,sd,se,sf,sg,sh,si,sj)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj
               ]
untypeType (Tup11Type a b c d e f g h i j k) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk
               ]
untypeType (Tup12Type a b c d e f g h i j k l) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               ]
untypeType (Tup13Type a b c d e f g h i j k l m) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               , untypeType m sm
               ]
untypeType (Tup14Type a b c d e f g h i j k l m n) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               , untypeType m sm, untypeType n sn
               ]
untypeType (Tup15Type a b c d e f g h i j k l m n o) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so)
  = Ut.TupType [ untypeType a sa, untypeType b sb, untypeType c sc
               , untypeType d sd, untypeType e se, untypeType f sf
               , untypeType g sg, untypeType h sh, untypeType i si
               , untypeType j sj, untypeType k sk, untypeType l sl
               , untypeType m sm, untypeType n sn, untypeType o so
               ]
untypeType (MutType a) sz           = Ut.MutType (untypeType a sz)
untypeType (RefType a) sz           = Ut.RefType (untypeType a sz)
untypeType (ArrayType a) (rs :> es) = Ut.ArrayType rs (untypeType a es)
untypeType (MArrType a) (rs :> es)  = Ut.MArrType rs (untypeType a es)
untypeType (ParType a) sz           = Ut.ParType (untypeType a sz)
untypeType (ElementsType a) (rs :> es) = Ut.ElementsType (untypeType a es)
untypeType (IVarType a) sz          = Ut.IVarType $ untypeType a sz
untypeType (FunType a b) (sa, sz)   = Ut.FunType (untypeType a sa) (untypeType b sz)
untypeType (FValType a) sz          = Ut.FValType (untypeType a sz)
untypeType typ _                    = error "untypeType: missing "

convSign :: Signedness a -> Ut.Signedness
convSign U       = Unsigned
convSign S       = Signed

convSize :: BitWidth a -> Ut.Size
convSize N8      = S8
convSize N16     = S16
convSize N32     = S32
convSize N64     = S64
convSize NNative = S32

literal :: TypeRep a -> Size a -> a -> Lit
literal t@UnitType        sz a = literalConst t sz a
literal t@BoolType        sz a = literalConst t sz a
literal t@IntType{}       sz a = literalConst t sz a
literal t@FloatType       sz a = literalConst t sz a
literal t@DoubleType      sz a = literalConst t sz a
literal t@ComplexType{}   sz a = literalConst t sz a
literal t@ArrayType{}     sz a = literalConst t sz a
literal (Tup2Type ta tb) (sa,sb) (a,b)
    = LTup [literal ta sa a, literal tb sb b]

literal (Tup3Type ta tb tc) (sa,sb,sc) (a,b,c)
    = LTup [literal ta sa a, literal tb sb b, literal tc sc c]

literal (Tup4Type ta tb tc td) (sa,sb,sc,sd) (a,b,c,d)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d
           ]

literal (Tup5Type ta tb tc td te) (sa,sb,sc,sd,se) (a,b,c,d,e)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d, literal te se e
           ]

literal (Tup6Type ta tb tc td te tf) (sa,sb,sc,sd,se,sf) (a,b,c,d,e,f)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d, literal te se e, literal tf sf f
           ]

literal (Tup7Type ta tb tc td te tf tg) (sa,sb,sc,sd,se,sf,sg) (a,b,c,d,e,f,g)
    = LTup [ literal ta sa a, literal tb sb b, literal tc sc c
           , literal td sd d, literal te se e, literal tf sf f
           , literal tg sg g
           ]
literal t s a = error "Missing pattern: FromTyped.hs: literal"

literalConst :: TypeRep a -> Size a -> a -> Lit
literalConst UnitType        _  ()     = LTup []
literalConst BoolType        _  a      = LBool a
literalConst (IntType s n)   sz a      = LInt (convSign s) (convSize n) (toInteger a)
literalConst FloatType       _  a      = LFloat a
literalConst DoubleType      _  a      = LDouble a
literalConst (ArrayType t)   _  a      = LArray t' $ map (literalConst t (defaultSize t)) a
  where t' = untypeType t (defaultSize t)
literalConst (ComplexType t) _  (r:+i) = LComplex re ie
  where re = literalConst t (defaultSize t) r
        ie = literalConst t (defaultSize t) i

-- | Construct a ValueInfo from a TypeRep and a Size
toValueInfo :: TypeRep a -> Size a -> ValueInfo
toValueInfo UnitType          _             = VIProd []
-- FIXME: No range for boolean types yet.
toValueInfo BoolType          _             = VIBool    (Range 0 1)
toValueInfo (IntType U N8)    r             = VIWord8   r
toValueInfo (IntType S N8)    r             = VIInt8    r
toValueInfo (IntType U N16)   r             = VIWord16  r
toValueInfo (IntType S N16)   r             = VIInt16   r
toValueInfo (IntType U N32)   r             = VIWord32  r
toValueInfo (IntType S N32)   r             = VIInt32   r
toValueInfo (IntType U N64)   r             = VIWord64  r
toValueInfo (IntType S N64)   r             = VIInt64   r
-- FIXME: No range for FP types and ComplexType yet.
toValueInfo FloatType         _             = VIFloat
toValueInfo DoubleType        _             = VIDouble
toValueInfo (ComplexType _)   _             = VIProd []
toValueInfo (Tup2Type a b) (sa,sb)
  = VIProd [toValueInfo a sa, toValueInfo b sb]
toValueInfo (Tup3Type a b c) (sa,sb,sc)
  = VIProd [toValueInfo a sa, toValueInfo b sb, toValueInfo c sc]
toValueInfo (Tup4Type a b c d) (sa,sb,sc,sd)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd
           ]
toValueInfo (Tup5Type a b c d e) (sa,sb,sc,sd,se)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se
           ]
toValueInfo (Tup6Type a b c d e f) (sa,sb,sc,sd,se,sf)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           ]
toValueInfo (Tup7Type a b c d e f g) (sa,sb,sc,sd,se,sf,sg)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg
           ]
toValueInfo (Tup8Type a b c d e f g h) (sa,sb,sc,sd,se,sf,sg,sh)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh
           ]
toValueInfo (Tup9Type a b c d e f g h i) (sa,sb,sc,sd,se,sf,sg,sh,si)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
            ]
toValueInfo (Tup10Type a b c d e f g h i j) (sa,sb,sc,sd,se,sf,sg,sh,si,sj)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj
           ]
toValueInfo (Tup11Type a b c d e f g h i j k) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk
           ]
toValueInfo (Tup12Type a b c d e f g h i j k l) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           ]
toValueInfo (Tup13Type a b c d e f g h i j k l m) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           , toValueInfo m sm
           ]
toValueInfo (Tup14Type a b c d e f g h i j k l m n) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           , toValueInfo m sm, toValueInfo n sn
           ]
toValueInfo (Tup15Type a b c d e f g h i j k l m n o) (sa,sb,sc,sd,se,sf,sg,sh,si,sj,sk,sl,sm,sn,so)
  = VIProd [ toValueInfo a sa, toValueInfo b sb, toValueInfo c sc
           , toValueInfo d sd, toValueInfo e se, toValueInfo f sf
           , toValueInfo g sg, toValueInfo h sh, toValueInfo i si
           , toValueInfo j sj, toValueInfo k sk, toValueInfo l sl
           , toValueInfo m sm, toValueInfo n sn, toValueInfo o so
           ]
toValueInfo (MutType a) sz                  = toValueInfo a sz
toValueInfo (RefType a) sz                  = toValueInfo a sz
toValueInfo (ArrayType a) ((Range (WordN l) (WordN r)) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (MArrType a) ((Range (WordN l) (WordN r)) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (ParType a) sz                  = toValueInfo a sz
toValueInfo (ElementsType a) ((Range (WordN l) (WordN r)) :> es)
  = VIProd [VIWord32 (Range l r), toValueInfo a es]
toValueInfo (IVarType a) sz                 = toValueInfo a sz
-- TODO: Maybe keep argument information for FunType.
toValueInfo (FunType a b) (sa, _)           = toValueInfo a sa
toValueInfo (FValType a) sz                 = toValueInfo a sz
toValueInfo typ _
  = error $ "toValueInfo: missing case for " ++ show typ
