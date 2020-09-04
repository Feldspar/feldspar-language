{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.NodeProto (NodeProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.AttributeProto as Onnx (AttributeProto)

data NodeProto = NodeProto{input :: !(P'.Seq P'.Utf8), output :: !(P'.Seq P'.Utf8), name :: !(P'.Maybe P'.Utf8),
                           op_type :: !(P'.Maybe P'.Utf8), domain :: !(P'.Maybe P'.Utf8),
                           attribute :: !(P'.Seq Onnx.AttributeProto), doc_string :: !(P'.Maybe P'.Utf8)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable NodeProto where
  mergeAppend (NodeProto x'1 x'2 x'3 x'4 x'5 x'6 x'7) (NodeProto y'1 y'2 y'3 y'4 y'5 y'6 y'7)
   = NodeProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)

instance P'.Default NodeProto where
  defaultValue
   = NodeProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire NodeProto where
  wireSize ft' self'@(NodeProto x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeRep 1 9 x'1 + P'.wireSizeRep 1 9 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 1 9 x'4 +
             P'.wireSizeOpt 1 9 x'5
             + P'.wireSizeRep 1 11 x'6
             + P'.wireSizeOpt 1 9 x'7)
  wirePutWithSize ft' self'@(NodeProto x'1 x'2 x'3 x'4 x'5 x'6 x'7)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 10 9 x'1, P'.wirePutRepWithSize 18 9 x'2, P'.wirePutOptWithSize 26 9 x'3,
             P'.wirePutOptWithSize 34 9 x'4, P'.wirePutRepWithSize 42 11 x'6, P'.wirePutOptWithSize 50 9 x'7,
             P'.wirePutOptWithSize 58 9 x'5]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{input = P'.append (input old'Self) new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{output = P'.append (output old'Self) new'Field}) (P'.wireGet 9)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{op_type = Prelude'.Just new'Field}) (P'.wireGet 9)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{domain = Prelude'.Just new'Field}) (P'.wireGet 9)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{attribute = P'.append (attribute old'Self) new'Field}) (P'.wireGet 11)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{doc_string = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> NodeProto) NodeProto where
  getVal m' f' = f' m'

instance P'.GPB NodeProto

instance P'.ReflectDescriptor NodeProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34, 42, 50, 58])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.NodeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"NodeProto\"}, descFilePath = [\"Onnx\",\"NodeProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.input\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"input\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.output\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"output\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.op_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"op_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.domain\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"domain\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.attribute\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"attribute\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.AttributeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"AttributeProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.NodeProto.doc_string\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"NodeProto\"], baseName' = FName \"doc_string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType NodeProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg NodeProto where
  textPut msg
   = do
       P'.tellT "input" (input msg)
       P'.tellT "output" (output msg)
       P'.tellT "name" (name msg)
       P'.tellT "op_type" (op_type msg)
       P'.tellT "domain" (domain msg)
       P'.tellT "attribute" (attribute msg)
       P'.tellT "doc_string" (doc_string msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice [parse'input, parse'output, parse'name, parse'op_type, parse'domain, parse'attribute, parse'doc_string])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
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
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'op_type
         = P'.try
            (do
               v <- P'.getT "op_type"
               Prelude'.return (\ o -> o{op_type = v}))
        parse'domain
         = P'.try
            (do
               v <- P'.getT "domain"
               Prelude'.return (\ o -> o{domain = v}))
        parse'attribute
         = P'.try
            (do
               v <- P'.getT "attribute"
               Prelude'.return (\ o -> o{attribute = P'.append (attribute o) v}))
        parse'doc_string
         = P'.try
            (do
               v <- P'.getT "doc_string"
               Prelude'.return (\ o -> o{doc_string = v}))