{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.ModelProto (ModelProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.GraphProto as Onnx (GraphProto)
import qualified Onnx.OperatorSetIdProto as Onnx (OperatorSetIdProto)
import qualified Onnx.StringStringEntryProto as Onnx (StringStringEntryProto)

data ModelProto = ModelProto{ir_version :: !(P'.Maybe P'.Int64), opset_import :: !(P'.Seq Onnx.OperatorSetIdProto),
                             producer_name :: !(P'.Maybe P'.Utf8), producer_version :: !(P'.Maybe P'.Utf8),
                             domain :: !(P'.Maybe P'.Utf8), model_version :: !(P'.Maybe P'.Int64),
                             doc_string :: !(P'.Maybe P'.Utf8), graph :: !(P'.Maybe Onnx.GraphProto),
                             metadata_props :: !(P'.Seq Onnx.StringStringEntryProto)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable ModelProto where
  mergeAppend (ModelProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (ModelProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = ModelProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)

instance P'.Default ModelProto where
  defaultValue
   = ModelProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire ModelProto where
  wireSize ft' self'@(ModelProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 3 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 1 9 x'4 +
             P'.wireSizeOpt 1 9 x'5
             + P'.wireSizeOpt 1 3 x'6
             + P'.wireSizeOpt 1 9 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeRep 1 11 x'9)
  wirePutWithSize ft' self'@(ModelProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 8 3 x'1, P'.wirePutOptWithSize 18 9 x'3, P'.wirePutOptWithSize 26 9 x'4,
             P'.wirePutOptWithSize 34 9 x'5, P'.wirePutOptWithSize 40 3 x'6, P'.wirePutOptWithSize 50 9 x'7,
             P'.wirePutOptWithSize 58 11 x'8, P'.wirePutRepWithSize 66 11 x'2, P'.wirePutRepWithSize 114 11 x'9]
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{ir_version = Prelude'.Just new'Field}) (P'.wireGet 3)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{opset_import = P'.append (opset_import old'Self) new'Field})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{producer_name = Prelude'.Just new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{producer_version = Prelude'.Just new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{domain = Prelude'.Just new'Field}) (P'.wireGet 9)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{model_version = Prelude'.Just new'Field}) (P'.wireGet 3)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{doc_string = Prelude'.Just new'Field}) (P'.wireGet 9)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{graph = P'.mergeAppend (graph old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             114 -> Prelude'.fmap (\ !new'Field -> old'Self{metadata_props = P'.append (metadata_props old'Self) new'Field})
                     (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> ModelProto) ModelProto where
  getVal m' f' = f' m'

instance P'.GPB ModelProto

instance P'.ReflectDescriptor ModelProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 26, 34, 40, 50, 58, 66, 114])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.ModelProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"ModelProto\"}, descFilePath = [\"Onnx\",\"ModelProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.ir_version\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"ir_version\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.opset_import\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"opset_import\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.OperatorSetIdProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"OperatorSetIdProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.producer_name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"producer_name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.producer_version\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"producer_version\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.domain\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"domain\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.model_version\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"model_version\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.doc_string\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"doc_string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.graph\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"graph\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.GraphProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"GraphProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.ModelProto.metadata_props\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"ModelProto\"], baseName' = FName \"metadata_props\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 114}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.StringStringEntryProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"StringStringEntryProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType ModelProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg ModelProto where
  textPut msg
   = do
       P'.tellT "ir_version" (ir_version msg)
       P'.tellT "opset_import" (opset_import msg)
       P'.tellT "producer_name" (producer_name msg)
       P'.tellT "producer_version" (producer_version msg)
       P'.tellT "domain" (domain msg)
       P'.tellT "model_version" (model_version msg)
       P'.tellT "doc_string" (doc_string msg)
       P'.tellT "graph" (graph msg)
       P'.tellT "metadata_props" (metadata_props msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'ir_version, parse'opset_import, parse'producer_name, parse'producer_version, parse'domain,
                   parse'model_version, parse'doc_string, parse'graph, parse'metadata_props])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'ir_version
         = P'.try
            (do
               v <- P'.getT "ir_version"
               Prelude'.return (\ o -> o{ir_version = v}))
        parse'opset_import
         = P'.try
            (do
               v <- P'.getT "opset_import"
               Prelude'.return (\ o -> o{opset_import = P'.append (opset_import o) v}))
        parse'producer_name
         = P'.try
            (do
               v <- P'.getT "producer_name"
               Prelude'.return (\ o -> o{producer_name = v}))
        parse'producer_version
         = P'.try
            (do
               v <- P'.getT "producer_version"
               Prelude'.return (\ o -> o{producer_version = v}))
        parse'domain
         = P'.try
            (do
               v <- P'.getT "domain"
               Prelude'.return (\ o -> o{domain = v}))
        parse'model_version
         = P'.try
            (do
               v <- P'.getT "model_version"
               Prelude'.return (\ o -> o{model_version = v}))
        parse'doc_string
         = P'.try
            (do
               v <- P'.getT "doc_string"
               Prelude'.return (\ o -> o{doc_string = v}))
        parse'graph
         = P'.try
            (do
               v <- P'.getT "graph"
               Prelude'.return (\ o -> o{graph = v}))
        parse'metadata_props
         = P'.try
            (do
               v <- P'.getT "metadata_props"
               Prelude'.return (\ o -> o{metadata_props = P'.append (metadata_props o) v}))