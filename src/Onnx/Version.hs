{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.Version (Version(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Version = U'START_VERSION
             | IR_VERSION_2017_10_10
             | IR_VERSION_2017_10_30
             | IR_VERSION_2017_11_3
             | IR_VERSION_2019_1_22
             | IR_VERSION_2019_3_18
             | IR_VERSION
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                         Prelude'.Generic)

instance P'.Mergeable Version

instance Prelude'.Bounded Version where
  minBound = U'START_VERSION
  maxBound = IR_VERSION

instance P'.Default Version where
  defaultValue = U'START_VERSION

toMaybe'Enum :: Prelude'.Int -> P'.Maybe Version
toMaybe'Enum 0 = Prelude'.Just U'START_VERSION
toMaybe'Enum 1 = Prelude'.Just IR_VERSION_2017_10_10
toMaybe'Enum 2 = Prelude'.Just IR_VERSION_2017_10_30
toMaybe'Enum 3 = Prelude'.Just IR_VERSION_2017_11_3
toMaybe'Enum 4 = Prelude'.Just IR_VERSION_2019_1_22
toMaybe'Enum 5 = Prelude'.Just IR_VERSION_2019_3_18
toMaybe'Enum 6 = Prelude'.Just IR_VERSION
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum Version where
  fromEnum U'START_VERSION = 0
  fromEnum IR_VERSION_2017_10_10 = 1
  fromEnum IR_VERSION_2017_10_30 = 2
  fromEnum IR_VERSION_2017_11_3 = 3
  fromEnum IR_VERSION_2019_1_22 = 4
  fromEnum IR_VERSION_2019_3_18 = 5
  fromEnum IR_VERSION = 6
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Onnx.Version") . toMaybe'Enum
  succ U'START_VERSION = IR_VERSION_2017_10_10
  succ IR_VERSION_2017_10_10 = IR_VERSION_2017_10_30
  succ IR_VERSION_2017_10_30 = IR_VERSION_2017_11_3
  succ IR_VERSION_2017_11_3 = IR_VERSION_2019_1_22
  succ IR_VERSION_2019_1_22 = IR_VERSION_2019_3_18
  succ IR_VERSION_2019_3_18 = IR_VERSION
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Onnx.Version"
  pred IR_VERSION_2017_10_10 = U'START_VERSION
  pred IR_VERSION_2017_10_30 = IR_VERSION_2017_10_10
  pred IR_VERSION_2017_11_3 = IR_VERSION_2017_10_30
  pred IR_VERSION_2019_1_22 = IR_VERSION_2017_11_3
  pred IR_VERSION_2019_3_18 = IR_VERSION_2019_1_22
  pred IR_VERSION = IR_VERSION_2019_3_18
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Onnx.Version"

instance P'.Wire Version where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB Version

instance P'.MessageAPI msg' (msg' -> Version) Version where
  getVal m' f' = f' m'

instance P'.ReflectEnum Version where
  reflectEnum
   = [(0, "U'START_VERSION", U'START_VERSION), (1, "IR_VERSION_2017_10_10", IR_VERSION_2017_10_10),
      (2, "IR_VERSION_2017_10_30", IR_VERSION_2017_10_30), (3, "IR_VERSION_2017_11_3", IR_VERSION_2017_11_3),
      (4, "IR_VERSION_2019_1_22", IR_VERSION_2019_1_22), (5, "IR_VERSION_2019_3_18", IR_VERSION_2019_3_18),
      (6, "IR_VERSION", IR_VERSION)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".onnx.Version") [] ["Onnx"] "Version") ["Onnx", "Version.hs"]
      [(0, "U'START_VERSION"), (1, "IR_VERSION_2017_10_10"), (2, "IR_VERSION_2017_10_30"), (3, "IR_VERSION_2017_11_3"),
       (4, "IR_VERSION_2019_1_22"), (5, "IR_VERSION_2019_3_18"), (6, "IR_VERSION")]
      Prelude'.False

instance P'.TextType Version where
  tellT = P'.tellShow
  getT = P'.getRead