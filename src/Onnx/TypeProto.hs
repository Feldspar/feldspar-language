{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.TypeProto (TypeProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TypeProto.Value as Onnx.TypeProto (Value)
import qualified Onnx.TypeProto.Value as Onnx.TypeProto.Value (Value(..), get'tensor_type, get'sequence_type, get'map_type)

data TypeProto = TypeProto{denotation :: !(P'.Maybe P'.Utf8), value :: P'.Maybe (Onnx.TypeProto.Value)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TypeProto where
  mergeAppend (TypeProto x'1 x'2) (TypeProto y'1 y'2) = TypeProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default TypeProto where
  defaultValue = TypeProto P'.defaultValue P'.defaultValue

instance P'.Wire TypeProto where
  wireSize ft' self'@(TypeProto x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 11 (Onnx.TypeProto.Value.get'tensor_type Prelude'.=<< x'2) +
             P'.wireSizeOpt 1 11 (Onnx.TypeProto.Value.get'sequence_type Prelude'.=<< x'2)
             + P'.wireSizeOpt 1 11 (Onnx.TypeProto.Value.get'map_type Prelude'.=<< x'2))
  wirePutWithSize ft' self'@(TypeProto x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 11 (Onnx.TypeProto.Value.get'tensor_type Prelude'.=<< x'2),
             P'.wirePutOptWithSize 34 11 (Onnx.TypeProto.Value.get'sequence_type Prelude'.=<< x'2),
             P'.wirePutOptWithSize 42 11 (Onnx.TypeProto.Value.get'map_type Prelude'.=<< x'2), P'.wirePutOptWithSize 50 9 x'1]
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
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{denotation = Prelude'.Just new'Field}) (P'.wireGet 9)
             10 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{value =
                                P'.mergeAppend (value old'Self) (Prelude'.Just (Onnx.TypeProto.Value.Tensor_type new'Field))})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{value =
                                P'.mergeAppend (value old'Self) (Prelude'.Just (Onnx.TypeProto.Value.Sequence_type new'Field))})
                    (P'.wireGet 11)
             42 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{value = P'.mergeAppend (value old'Self) (Prelude'.Just (Onnx.TypeProto.Value.Map_type new'Field))})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TypeProto) TypeProto where
  getVal m' f' = f' m'

instance P'.GPB TypeProto

instance P'.ReflectDescriptor TypeProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TypeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TypeProto\"}, descFilePath = [\"Onnx\",\"TypeProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.denotation\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\"], baseName' = FName \"denotation\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [OneofInfo {oneofName = ProtoName {protobufName = FIName \".onnx.TypeProto.value\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Value\"}, oneofFName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.value\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\"], baseName' = FName \"value\", baseNamePrefix' = \"\"}, oneofFilePath = [\"Onnx\",\"TypeProto\",\"Value.hs\"], oneofFields = fromList [(ProtoName {protobufName = FIName \".onnx.TypeProto.value.tensor_type\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\",MName \"Value\"], baseName = MName \"Tensor_type\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.value.tensor_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Value\"], baseName' = FName \"tensor_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TypeProto.Tensor\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Tensor\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".onnx.TypeProto.value.sequence_type\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\",MName \"Value\"], baseName = MName \"Sequence_type\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.value.sequence_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Value\"], baseName' = FName \"sequence_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TypeProto.Sequence\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Sequence\"}), hsRawDefault = Nothing, hsDefault = Nothing}),(ProtoName {protobufName = FIName \".onnx.TypeProto.value.map_type\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\",MName \"Value\"], baseName = MName \"Map_type\"},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.value.map_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Value\"], baseName' = FName \"map_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TypeProto.Map\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Map\"}), hsRawDefault = Nothing, hsDefault = Nothing})], oneofMakeLenses = False}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType TypeProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TypeProto where
  textPut msg
   = do
       P'.tellT "denotation" (denotation msg)
       case (value msg) of
         Prelude'.Just (Onnx.TypeProto.Value.Tensor_type tensor_type) -> P'.tellT "tensor_type" tensor_type
         Prelude'.Just (Onnx.TypeProto.Value.Sequence_type sequence_type) -> P'.tellT "sequence_type" sequence_type
         Prelude'.Just (Onnx.TypeProto.Value.Map_type map_type) -> P'.tellT "map_type" map_type
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
        parse'value = P'.try (P'.choice [parse'tensor_type, parse'sequence_type, parse'map_type])
          where
              parse'tensor_type
               = P'.try
                  (do
                     v <- P'.getT "tensor_type"
                     Prelude'.return (\ s -> s{value = Prelude'.Just (Onnx.TypeProto.Value.Tensor_type v)}))
              parse'sequence_type
               = P'.try
                  (do
                     v <- P'.getT "sequence_type"
                     Prelude'.return (\ s -> s{value = Prelude'.Just (Onnx.TypeProto.Value.Sequence_type v)}))
              parse'map_type
               = P'.try
                  (do
                     v <- P'.getT "map_type"
                     Prelude'.return (\ s -> s{value = Prelude'.Just (Onnx.TypeProto.Value.Map_type v)}))