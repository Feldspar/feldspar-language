{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TypeProto.Map (Map(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import {-# SOURCE #-} qualified Onnx.TypeProto as Onnx (TypeProto)

data Map = Map{key_type :: !(P'.Maybe P'.Int32), value_type :: !(P'.Maybe Onnx.TypeProto)}
           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Map where
  mergeAppend (Map x'1 x'2) (Map y'1 y'2) = Map (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Map where
  defaultValue = Map P'.defaultValue P'.defaultValue

instance P'.Wire Map where
  wireSize ft' self'@(Map x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 11 x'2)
  wirePutWithSize ft' self'@(Map x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutOptWithSize 8 5 x'1, P'.wirePutOptWithSize 18 11 x'2]
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{key_type = Prelude'.Just new'Field}) (P'.wireGet 5)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{value_type = P'.mergeAppend (value_type old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Map) Map where
  getVal m' f' = f' m'

instance P'.GPB Map

instance P'.ReflectDescriptor Map where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TypeProto.Map\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Map\"}, descFilePath = [\"Onnx\",\"TypeProto\",\"Map.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.Map.key_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Map\"], baseName' = FName \"key_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.Map.value_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Map\"], baseName' = FName \"value_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TypeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TypeProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Map where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Map where
  textPut msg
   = do
       P'.tellT "key_type" (key_type msg)
       P'.tellT "value_type" (value_type msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'key_type, parse'value_type]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'key_type
         = P'.try
            (do
               v <- P'.getT "key_type"
               Prelude'.return (\ o -> o{key_type = v}))
        parse'value_type
         = P'.try
            (do
               v <- P'.getT "value_type"
               Prelude'.return (\ o -> o{value_type = v}))