{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorShapeProto.Dimension (Dimension(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TensorShapeProto.Dimension.Value as Onnx.TensorShapeProto.Dimension (Value)
import qualified Onnx.TensorShapeProto.Dimension.Value as Onnx.TensorShapeProto.Dimension.Value
       (Value(..), get'dim_value, get'dim_param)

data Dimension = Dimension{denotation :: !(P'.Maybe P'.Utf8), value :: P'.Maybe (Onnx.TensorShapeProto.Dimension.Value)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Dimension where
  mergeAppend (Dimension x'1 x'2) (Dimension y'1 y'2) = Dimension (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Dimension where
  defaultValue = Dimension P'.defaultValue P'.defaultValue

instance P'.Wire Dimension where
  wireSize ft' self'@(Dimension x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 3 (Onnx.TensorShapeProto.Dimension.Value.get'dim_value Prelude'.=<< x'2) +
             P'.wireSizeOpt 1 9 (Onnx.TensorShapeProto.Dimension.Value.get'dim_param Prelude'.=<< x'2))
  wirePutWithSize ft' self'@(Dimension x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 8 3 (Onnx.TensorShapeProto.Dimension.Value.get'dim_value Prelude'.=<< x'2),
             P'.wirePutOptWithSize 18 9 (Onnx.TensorShapeProto.Dimension.Value.get'dim_param Prelude'.=<< x'2),
             P'.wirePutOptWithSize 26 9 x'1]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith (P'.catch'Unknown' P'.discardUnknown update'Self)
       11 -> P'.getMessageWith (P'.catch'Unknown' P'.discardUnknown update'Self)
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{denotation = Prelude'.Just new'Field}) (P'.wireGet 9)
             8 -> Prelude'.fmap
                   (\ !new'Field -> old'Self{value = Prelude'.Just (Onnx.TensorShapeProto.Dimension.Value.Dim_value new'Field)})
                   (P'.wireGet 3)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{value = Prelude'.Just (Onnx.TensorShapeProto.Dimension.Value.Dim_param new'Field)})
                    (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Dimension) Dimension where
  getVal m' f' = f' m'

instance P'.GPB Dimension

instance P'.ReflectDescriptor Dimension where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TensorShapeProto.Dimension\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorShapeProto\"], baseName = MName \"Dimension\"}, descFilePath = [\"Onnx\",\"TensorShapeProto\",\"Dimension.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorShapeProto.Dimension.denotation\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\"], baseName' = FName \"denotation\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [OneofInfo {oneofName = ProtoName {protobufName = FIName \".onnx.TensorShapeProto.Dimension.value\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\"], baseName = MName \"Value\"}, oneofFName = ProtoFName {protobufName' = FIName \".onnx.TensorShapeProto.Dimension.value\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\"], baseName' = FName \"value\", baseNamePrefix' = \"\"}, oneofFilePath = [\"Onnx\",\"TensorShapeProto\",\"Dimension\",\"Value.hs\"], oneofFields = fromList [(ProtoName {protobufName = FIName \".onnx.TensorShapeProto.Dimension.value.dim_value\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\",MName \"Value\"], baseName = MName \"Dim_value\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorShapeProto.Dimension.value.dim_value\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\",MName \"Value\"], baseName' = FName \"dim_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".onnx.TensorShapeProto.Dimension.value.dim_param\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\",MName \"Value\"], baseName = MName \"Dim_param\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorShapeProto.Dimension.value.dim_param\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorShapeProto\",MName \"Dimension\",MName \"Value\"], baseName' = FName \"dim_param\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing})], oneofMakeLenses = False}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Dimension where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Dimension where
  textPut msg
   = do
       P'.tellT "denotation" (denotation msg)
       case (value msg) of
         Prelude'.Just (Onnx.TensorShapeProto.Dimension.Value.Dim_value dim_value) -> P'.tellT "dim_value" dim_value
         Prelude'.Just (Onnx.TensorShapeProto.Dimension.Value.Dim_param dim_param) -> P'.tellT "dim_param" dim_param
         Prelude'.Nothing -> Prelude'.return ()
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'denotation, parse'value]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'denotation
         = P'.try
            (do
               v <- P'.getT "denotation"
               Prelude'.return (\ o -> o{denotation = v}))
        parse'value = P'.try (P'.choice [parse'dim_value, parse'dim_param])
          where
              parse'dim_value
               = P'.try
                  (do
                     v <- P'.getT "dim_value"
                     Prelude'.return (\ s -> s{value = Prelude'.Just (Onnx.TensorShapeProto.Dimension.Value.Dim_value v)}))
              parse'dim_param
               = P'.try
                  (do
                     v <- P'.getT "dim_param"
                     Prelude'.return (\ s -> s{value = Prelude'.Just (Onnx.TensorShapeProto.Dimension.Value.Dim_param v)}))