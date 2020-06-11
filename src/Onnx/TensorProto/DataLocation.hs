{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorProto.DataLocation (DataLocation(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data DataLocation = DEFAULT
                  | EXTERNAL
                    deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                              Prelude'.Generic)

instance P'.Mergeable DataLocation

instance Prelude'.Bounded DataLocation where
  minBound = DEFAULT
  maxBound = EXTERNAL

instance P'.Default DataLocation where
  defaultValue = DEFAULT

toMaybe'Enum :: Prelude'.Int -> P'.Maybe DataLocation
toMaybe'Enum 0 = Prelude'.Just DEFAULT
toMaybe'Enum 1 = Prelude'.Just EXTERNAL
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum DataLocation where
  fromEnum DEFAULT = 0
  fromEnum EXTERNAL = 1
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Onnx.TensorProto.DataLocation") . toMaybe'Enum
  succ DEFAULT = EXTERNAL
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Onnx.TensorProto.DataLocation"
  pred EXTERNAL = DEFAULT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Onnx.TensorProto.DataLocation"

instance P'.Wire DataLocation where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB DataLocation

instance P'.MessageAPI msg' (msg' -> DataLocation) DataLocation where
  getVal m' f' = f' m'

instance P'.ReflectEnum DataLocation where
  reflectEnum = [(0, "DEFAULT", DEFAULT), (1, "EXTERNAL", EXTERNAL)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".onnx.TensorProto.DataLocation") [] ["Onnx", "TensorProto"] "DataLocation")
      ["Onnx", "TensorProto", "DataLocation.hs"]
      [(0, "DEFAULT"), (1, "EXTERNAL")]
      Prelude'.False

instance P'.TextType DataLocation where
  tellT = P'.tellShow
  getT = P'.getRead