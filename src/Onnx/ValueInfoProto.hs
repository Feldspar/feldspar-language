{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.ValueInfoProto (ValueInfoProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TypeProto as Onnx (TypeProto)

data ValueInfoProto = ValueInfoProto{name :: !(P'.Maybe P'.Utf8), type' :: !(P'.Maybe Onnx.TypeProto),
                                     doc_string :: !(P'.Maybe P'.Utf8)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable ValueInfoProto where
  mergeAppend (ValueInfoProto x'1 x'2 x'3) (ValueInfoProto y'1 y'2 y'3)
   = ValueInfoProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)

instance P'.Default ValueInfoProto where
  defaultValue = ValueInfoProto P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire ValueInfoProto where
  wireSize ft' self'@(ValueInfoProto x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 9 x'3)
  wirePutWithSize ft' self'@(ValueInfoProto x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize [P'.wirePutOptWithSize 10 9 x'1, P'.wirePutOptWithSize 18 11 x'2, P'.wirePutOptWithSize 26 9 x'3]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = P'.mergeAppend (type' old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{doc_string = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> ValueInfoProto) ValueInfoProto where
  getVal m' f' = f' m'

instance P'.GPB ValueInfoProto

instance P'.ReflectDescriptor ValueInfoProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.ValueInfoProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"ValueInfoProto\"}, descFilePath = [\"Onnx\",\"ValueInfoProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ValueInfoProto.name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ValueInfoProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ValueInfoProto.type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ValueInfoProto\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TypeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TypeProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ValueInfoProto.doc_string\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ValueInfoProto\"], baseName' = FName \"doc_string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType ValueInfoProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg ValueInfoProto where
  textPut msg
   = do
       P'.tellT "name" (name msg)
       P'.tellT "type" (type' msg)
       P'.tellT "doc_string" (doc_string msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'name, parse'type', parse'doc_string]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'doc_string
         = P'.try
            (do
               v <- P'.getT "doc_string"
               Prelude'.return (\ o -> o{doc_string = v}))