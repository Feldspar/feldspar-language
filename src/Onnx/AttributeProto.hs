{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.AttributeProto (AttributeProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.AttributeProto.AttributeType as Onnx.AttributeProto (AttributeType)
import qualified Onnx.GraphProto as Onnx (GraphProto)
import qualified Onnx.SparseTensorProto as Onnx (SparseTensorProto)
import qualified Onnx.TensorProto as Onnx (TensorProto)

data AttributeProto = AttributeProto{name :: !(P'.Maybe P'.Utf8), ref_attr_name :: !(P'.Maybe P'.Utf8),
                                     doc_string :: !(P'.Maybe P'.Utf8), type' :: !(P'.Maybe Onnx.AttributeProto.AttributeType),
                                     f :: !(P'.Maybe P'.Float), i :: !(P'.Maybe P'.Int64), s :: !(P'.Maybe P'.ByteString),
                                     t :: !(P'.Maybe Onnx.TensorProto), g :: !(P'.Maybe Onnx.GraphProto),
                                     sparse_tensor :: !(P'.Maybe Onnx.SparseTensorProto), floats :: !(P'.Seq P'.Float),
                                     ints :: !(P'.Seq P'.Int64), strings :: !(P'.Seq P'.ByteString),
                                     tensors :: !(P'.Seq Onnx.TensorProto), graphs :: !(P'.Seq Onnx.GraphProto),
                                     sparse_tensors :: !(P'.Seq Onnx.SparseTensorProto)}
                      deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable AttributeProto where
  mergeAppend (AttributeProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16)
   (AttributeProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14 y'15 y'16)
   = AttributeProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
      (P'.mergeAppend x'11 y'11)
      (P'.mergeAppend x'12 y'12)
      (P'.mergeAppend x'13 y'13)
      (P'.mergeAppend x'14 y'14)
      (P'.mergeAppend x'15 y'15)
      (P'.mergeAppend x'16 y'16)

instance P'.Default AttributeProto where
  defaultValue
   = AttributeProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire AttributeProto where
  wireSize ft' self'@(AttributeProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeOpt 2 9 x'2 + P'.wireSizeOpt 1 9 x'3 + P'.wireSizeOpt 2 14 x'4 +
             P'.wireSizeOpt 1 2 x'5
             + P'.wireSizeOpt 1 3 x'6
             + P'.wireSizeOpt 1 12 x'7
             + P'.wireSizeOpt 1 11 x'8
             + P'.wireSizeOpt 1 11 x'9
             + P'.wireSizeOpt 2 11 x'10
             + P'.wireSizeRep 1 2 x'11
             + P'.wireSizeRep 1 3 x'12
             + P'.wireSizeRep 1 12 x'13
             + P'.wireSizeRep 1 11 x'14
             + P'.wireSizeRep 1 11 x'15
             + P'.wireSizeRep 2 11 x'16)
  wirePutWithSize ft' self'@(AttributeProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14 x'15 x'16)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 9 x'1, P'.wirePutOptWithSize 21 2 x'5, P'.wirePutOptWithSize 24 3 x'6,
             P'.wirePutOptWithSize 34 12 x'7, P'.wirePutOptWithSize 42 11 x'8, P'.wirePutOptWithSize 50 11 x'9,
             P'.wirePutRepWithSize 61 2 x'11, P'.wirePutRepWithSize 64 3 x'12, P'.wirePutRepWithSize 74 12 x'13,
             P'.wirePutRepWithSize 82 11 x'14, P'.wirePutRepWithSize 90 11 x'15, P'.wirePutOptWithSize 106 9 x'3,
             P'.wirePutOptWithSize 160 14 x'4, P'.wirePutOptWithSize 170 9 x'2, P'.wirePutOptWithSize 178 11 x'10,
             P'.wirePutRepWithSize 186 11 x'16]
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
             170 -> Prelude'.fmap (\ !new'Field -> old'Self{ref_attr_name = Prelude'.Just new'Field}) (P'.wireGet 9)
             106 -> Prelude'.fmap (\ !new'Field -> old'Self{doc_string = Prelude'.Just new'Field}) (P'.wireGet 9)
             160 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = Prelude'.Just new'Field}) (P'.wireGet 14)
             21 -> Prelude'.fmap (\ !new'Field -> old'Self{f = Prelude'.Just new'Field}) (P'.wireGet 2)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{i = Prelude'.Just new'Field}) (P'.wireGet 3)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{s = Prelude'.Just new'Field}) (P'.wireGet 12)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{t = P'.mergeAppend (t old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{g = P'.mergeAppend (g old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             178 -> Prelude'.fmap
                     (\ !new'Field -> old'Self{sparse_tensor = P'.mergeAppend (sparse_tensor old'Self) (Prelude'.Just new'Field)})
                     (P'.wireGet 11)
             61 -> Prelude'.fmap (\ !new'Field -> old'Self{floats = P'.append (floats old'Self) new'Field}) (P'.wireGet 2)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{floats = P'.mergeAppend (floats old'Self) new'Field})
                    (P'.wireGetPacked 2)
             64 -> Prelude'.fmap (\ !new'Field -> old'Self{ints = P'.append (ints old'Self) new'Field}) (P'.wireGet 3)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{ints = P'.mergeAppend (ints old'Self) new'Field}) (P'.wireGetPacked 3)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{strings = P'.append (strings old'Self) new'Field}) (P'.wireGet 12)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{tensors = P'.append (tensors old'Self) new'Field}) (P'.wireGet 11)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{graphs = P'.append (graphs old'Self) new'Field}) (P'.wireGet 11)
             186 -> Prelude'.fmap (\ !new'Field -> old'Self{sparse_tensors = P'.append (sparse_tensors old'Self) new'Field})
                     (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> AttributeProto) AttributeProto where
  getVal m' f' = f' m'

instance P'.GPB AttributeProto

instance P'.ReflectDescriptor AttributeProto where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList [10, 21, 24, 34, 42, 50, 58, 61, 64, 66, 74, 82, 90, 106, 160, 170, 178, 186])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.AttributeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"AttributeProto\"}, descFilePath = [\"Onnx\",\"AttributeProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.ref_attr_name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"ref_attr_name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 21}, wireTag = WireTag {getWireTag = 170}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.doc_string\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"doc_string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 106}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".onnx.AttributeProto.AttributeType\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"AttributeProto\"], baseName = MName \"AttributeType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.f\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"f\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 21}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.i\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"i\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.s\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"s\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.t\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"t\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.g\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"g\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.GraphProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"GraphProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.sparse_tensor\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"sparse_tensor\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 22}, wireTag = WireTag {getWireTag = 178}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.SparseTensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"SparseTensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.floats\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"floats\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 61}, packedTag = Just (WireTag {getWireTag = 61},WireTag {getWireTag = 58}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.ints\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"ints\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 64}, packedTag = Just (WireTag {getWireTag = 64},WireTag {getWireTag = 66}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.strings\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"strings\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.tensors\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"tensors\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.graphs\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"graphs\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.GraphProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"GraphProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.AttributeProto.sparse_tensors\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"AttributeProto\"], baseName' = FName \"sparse_tensors\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 186}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.SparseTensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"SparseTensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType AttributeProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg AttributeProto where
  textPut msg
   = do
       P'.tellT "name" (name msg)
       P'.tellT "ref_attr_name" (ref_attr_name msg)
       P'.tellT "doc_string" (doc_string msg)
       P'.tellT "type" (type' msg)
       P'.tellT "f" (f msg)
       P'.tellT "i" (i msg)
       P'.tellT "s" (s msg)
       P'.tellT "t" (t msg)
       P'.tellT "g" (g msg)
       P'.tellT "sparse_tensor" (sparse_tensor msg)
       P'.tellT "floats" (floats msg)
       P'.tellT "ints" (ints msg)
       P'.tellT "strings" (strings msg)
       P'.tellT "tensors" (tensors msg)
       P'.tellT "graphs" (graphs msg)
       P'.tellT "sparse_tensors" (sparse_tensors msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'name, parse'ref_attr_name, parse'doc_string, parse'type', parse'f, parse'i, parse's, parse't, parse'g,
                   parse'sparse_tensor, parse'floats, parse'ints, parse'strings, parse'tensors, parse'graphs, parse'sparse_tensors])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'ref_attr_name
         = P'.try
            (do
               v <- P'.getT "ref_attr_name"
               Prelude'.return (\ o -> o{ref_attr_name = v}))
        parse'doc_string
         = P'.try
            (do
               v <- P'.getT "doc_string"
               Prelude'.return (\ o -> o{doc_string = v}))
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'f
         = P'.try
            (do
               v <- P'.getT "f"
               Prelude'.return (\ o -> o{f = v}))
        parse'i
         = P'.try
            (do
               v <- P'.getT "i"
               Prelude'.return (\ o -> o{i = v}))
        parse's
         = P'.try
            (do
               v <- P'.getT "s"
               Prelude'.return (\ o -> o{s = v}))
        parse't
         = P'.try
            (do
               v <- P'.getT "t"
               Prelude'.return (\ o -> o{t = v}))
        parse'g
         = P'.try
            (do
               v <- P'.getT "g"
               Prelude'.return (\ o -> o{g = v}))
        parse'sparse_tensor
         = P'.try
            (do
               v <- P'.getT "sparse_tensor"
               Prelude'.return (\ o -> o{sparse_tensor = v}))
        parse'floats
         = P'.try
            (do
               v <- P'.getT "floats"
               Prelude'.return (\ o -> o{floats = P'.append (floats o) v}))
        parse'ints
         = P'.try
            (do
               v <- P'.getT "ints"
               Prelude'.return (\ o -> o{ints = P'.append (ints o) v}))
        parse'strings
         = P'.try
            (do
               v <- P'.getT "strings"
               Prelude'.return (\ o -> o{strings = P'.append (strings o) v}))
        parse'tensors
         = P'.try
            (do
               v <- P'.getT "tensors"
               Prelude'.return (\ o -> o{tensors = P'.append (tensors o) v}))
        parse'graphs
         = P'.try
            (do
               v <- P'.getT "graphs"
               Prelude'.return (\ o -> o{graphs = P'.append (graphs o) v}))
        parse'sparse_tensors
         = P'.try
            (do
               v <- P'.getT "sparse_tensors"
               Prelude'.return (\ o -> o{sparse_tensors = P'.append (sparse_tensors o) v}))