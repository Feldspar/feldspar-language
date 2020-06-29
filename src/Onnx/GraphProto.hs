{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.GraphProto (GraphProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import {-# SOURCE #-} qualified Onnx.NodeProto as Onnx (NodeProto)
import qualified Onnx.SparseTensorProto as Onnx (SparseTensorProto)
import qualified Onnx.TensorAnnotation as Onnx (TensorAnnotation)
import qualified Onnx.TensorProto as Onnx (TensorProto)
import qualified Onnx.ValueInfoProto as Onnx (ValueInfoProto)

data GraphProto = GraphProto{node :: !(P'.Seq Onnx.NodeProto), name :: !(P'.Maybe P'.Utf8),
                             initializer :: !(P'.Seq Onnx.TensorProto), sparse_initializer :: !(P'.Seq Onnx.SparseTensorProto),
                             doc_string :: !(P'.Maybe P'.Utf8), input :: !(P'.Seq Onnx.ValueInfoProto),
                             output :: !(P'.Seq Onnx.ValueInfoProto), value_info :: !(P'.Seq Onnx.ValueInfoProto),
                             quantization_annotation :: !(P'.Seq Onnx.TensorAnnotation)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable GraphProto where
  mergeAppend (GraphProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (GraphProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = GraphProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)

instance P'.Default GraphProto where
  defaultValue
   = GraphProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire GraphProto where
  wireSize ft' self'@(GraphProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeRep 1 11 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4 +
             P'.wireSizeOpt 1 9 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeRep 1 11 x'7
             + P'.wireSizeRep 1 11 x'8
             + P'.wireSizeRep 1 11 x'9)
  wirePutWithSize ft' self'@(GraphProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 10 11 x'1, P'.wirePutOptWithSize 18 9 x'2, P'.wirePutRepWithSize 42 11 x'3,
             P'.wirePutOptWithSize 82 9 x'5, P'.wirePutRepWithSize 90 11 x'6, P'.wirePutRepWithSize 98 11 x'7,
             P'.wirePutRepWithSize 106 11 x'8, P'.wirePutRepWithSize 114 11 x'9, P'.wirePutRepWithSize 122 11 x'4]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{node = P'.append (node old'Self) new'Field}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{initializer = P'.append (initializer old'Self) new'Field})
                    (P'.wireGet 11)
             122 -> Prelude'.fmap (\ !new'Field -> old'Self{sparse_initializer = P'.append (sparse_initializer old'Self) new'Field})
                     (P'.wireGet 11)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{doc_string = Prelude'.Just new'Field}) (P'.wireGet 9)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{input = P'.append (input old'Self) new'Field}) (P'.wireGet 11)
             98 -> Prelude'.fmap (\ !new'Field -> old'Self{output = P'.append (output old'Self) new'Field}) (P'.wireGet 11)
             106 -> Prelude'.fmap (\ !new'Field -> old'Self{value_info = P'.append (value_info old'Self) new'Field}) (P'.wireGet 11)
             114 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{quantization_annotation = P'.append (quantization_annotation old'Self) new'Field})
                     (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> GraphProto) GraphProto where
  getVal m' f' = f' m'

instance P'.GPB GraphProto

instance P'.ReflectDescriptor GraphProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 42, 82, 90, 98, 106, 114, 122])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.GraphProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"GraphProto\"}, descFilePath = [\"Onnx\",\"GraphProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.node\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"node\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.NodeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"NodeProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.initializer\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"initializer\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.sparse_initializer\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"sparse_initializer\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 15}, wireTag = WireTag {getWireTag = 122}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.SparseTensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"SparseTensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.doc_string\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"doc_string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.input\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"input\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.ValueInfoProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"ValueInfoProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.output\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"output\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 98}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.ValueInfoProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"ValueInfoProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.value_info\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"value_info\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 106}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.ValueInfoProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"ValueInfoProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.GraphProto.quantization_annotation\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"GraphProto\"], baseName' = FName \"quantization_annotation\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 114}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorAnnotation\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorAnnotation\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType GraphProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg GraphProto where
  textPut msg
   = do
       P'.tellT "node" (node msg)
       P'.tellT "name" (name msg)
       P'.tellT "initializer" (initializer msg)
       P'.tellT "sparse_initializer" (sparse_initializer msg)
       P'.tellT "doc_string" (doc_string msg)
       P'.tellT "input" (input msg)
       P'.tellT "output" (output msg)
       P'.tellT "value_info" (value_info msg)
       P'.tellT "quantization_annotation" (quantization_annotation msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'node, parse'name, parse'initializer, parse'sparse_initializer, parse'doc_string, parse'input, parse'output,
                   parse'value_info, parse'quantization_annotation])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'node
         = P'.try
            (do
               v <- P'.getT "node"
               Prelude'.return (\ o -> o{node = P'.append (node o) v}))
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'initializer
         = P'.try
            (do
               v <- P'.getT "initializer"
               Prelude'.return (\ o -> o{initializer = P'.append (initializer o) v}))
        parse'sparse_initializer
         = P'.try
            (do
               v <- P'.getT "sparse_initializer"
               Prelude'.return (\ o -> o{sparse_initializer = P'.append (sparse_initializer o) v}))
        parse'doc_string
         = P'.try
            (do
               v <- P'.getT "doc_string"
               Prelude'.return (\ o -> o{doc_string = v}))
        parse'input
         = P'.try
            (do
               v <- P'.getT "input"
               Prelude'.return (\ o -> o{input = P'.append (input o) v}))
        parse'output
         = P'.try
            (do
               v <- P'.getT "output"
               Prelude'.return (\ o -> o{output = P'.append (output o) v}))
        parse'value_info
         = P'.try
            (do
               v <- P'.getT "value_info"
               Prelude'.return (\ o -> o{value_info = P'.append (value_info o) v}))
        parse'quantization_annotation
         = P'.try
            (do
               v <- P'.getT "quantization_annotation"
               Prelude'.return (\ o -> o{quantization_annotation = P'.append (quantization_annotation o) v}))