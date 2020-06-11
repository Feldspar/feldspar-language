{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorProto (TensorProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.StringStringEntryProto as Onnx (StringStringEntryProto)
import qualified Onnx.TensorProto.DataLocation as Onnx.TensorProto (DataLocation)
import qualified Onnx.TensorProto.Segment as Onnx.TensorProto (Segment)

data TensorProto = TensorProto{dims :: !(P'.Seq P'.Int64), data_type :: !(P'.Maybe P'.Int32),
                               segment :: !(P'.Maybe Onnx.TensorProto.Segment), float_data :: !(P'.Seq P'.Float),
                               int32_data :: !(P'.Seq P'.Int32), string_data :: !(P'.Seq P'.ByteString),
                               int64_data :: !(P'.Seq P'.Int64), name :: !(P'.Maybe P'.Utf8), doc_string :: !(P'.Maybe P'.Utf8),
                               raw_data :: !(P'.Maybe P'.ByteString), external_data :: !(P'.Seq Onnx.StringStringEntryProto),
                               data_location :: !(P'.Maybe Onnx.TensorProto.DataLocation), double_data :: !(P'.Seq P'.Double),
                               uint64_data :: !(P'.Seq P'.Word64)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TensorProto where
  mergeAppend (TensorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14)
   (TensorProto y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10 y'11 y'12 y'13 y'14)
   = TensorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
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

instance P'.Default TensorProto where
  defaultValue
   = TensorProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue

instance P'.Wire TensorProto where
  wireSize ft' self'@(TensorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeRep 1 3 x'1 + P'.wireSizeOpt 1 5 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizePacked 1 2 x'4 +
             P'.wireSizePacked 1 5 x'5
             + P'.wireSizeRep 1 12 x'6
             + P'.wireSizePacked 1 3 x'7
             + P'.wireSizeOpt 1 9 x'8
             + P'.wireSizeOpt 1 9 x'9
             + P'.wireSizeOpt 1 12 x'10
             + P'.wireSizeRep 1 11 x'11
             + P'.wireSizeOpt 1 14 x'12
             + P'.wireSizePacked 1 1 x'13
             + P'.wireSizePacked 1 4 x'14)
  wirePutWithSize ft' self'@(TensorProto x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10 x'11 x'12 x'13 x'14)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutRepWithSize 8 3 x'1, P'.wirePutOptWithSize 16 5 x'2, P'.wirePutOptWithSize 26 11 x'3,
             P'.wirePutPackedWithSize 34 2 x'4, P'.wirePutPackedWithSize 42 5 x'5, P'.wirePutRepWithSize 50 12 x'6,
             P'.wirePutPackedWithSize 58 3 x'7, P'.wirePutOptWithSize 66 9 x'8, P'.wirePutOptWithSize 74 12 x'10,
             P'.wirePutPackedWithSize 82 1 x'13, P'.wirePutPackedWithSize 90 4 x'14, P'.wirePutOptWithSize 98 9 x'9,
             P'.wirePutRepWithSize 106 11 x'11, P'.wirePutOptWithSize 112 14 x'12]
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{dims = P'.append (dims old'Self) new'Field}) (P'.wireGet 3)
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{dims = P'.mergeAppend (dims old'Self) new'Field}) (P'.wireGetPacked 3)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{data_type = Prelude'.Just new'Field}) (P'.wireGet 5)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{segment = P'.mergeAppend (segment old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             37 -> Prelude'.fmap (\ !new'Field -> old'Self{float_data = P'.append (float_data old'Self) new'Field}) (P'.wireGet 2)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{float_data = P'.mergeAppend (float_data old'Self) new'Field})
                    (P'.wireGetPacked 2)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{int32_data = P'.append (int32_data old'Self) new'Field}) (P'.wireGet 5)
             42 -> Prelude'.fmap (\ !new'Field -> old'Self{int32_data = P'.mergeAppend (int32_data old'Self) new'Field})
                    (P'.wireGetPacked 5)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{string_data = P'.append (string_data old'Self) new'Field})
                    (P'.wireGet 12)
             56 -> Prelude'.fmap (\ !new'Field -> old'Self{int64_data = P'.append (int64_data old'Self) new'Field}) (P'.wireGet 3)
             58 -> Prelude'.fmap (\ !new'Field -> old'Self{int64_data = P'.mergeAppend (int64_data old'Self) new'Field})
                    (P'.wireGetPacked 3)
             66 -> Prelude'.fmap (\ !new'Field -> old'Self{name = Prelude'.Just new'Field}) (P'.wireGet 9)
             98 -> Prelude'.fmap (\ !new'Field -> old'Self{doc_string = Prelude'.Just new'Field}) (P'.wireGet 9)
             74 -> Prelude'.fmap (\ !new'Field -> old'Self{raw_data = Prelude'.Just new'Field}) (P'.wireGet 12)
             106 -> Prelude'.fmap (\ !new'Field -> old'Self{external_data = P'.append (external_data old'Self) new'Field})
                     (P'.wireGet 11)
             112 -> Prelude'.fmap (\ !new'Field -> old'Self{data_location = Prelude'.Just new'Field}) (P'.wireGet 14)
             81 -> Prelude'.fmap (\ !new'Field -> old'Self{double_data = P'.append (double_data old'Self) new'Field}) (P'.wireGet 1)
             82 -> Prelude'.fmap (\ !new'Field -> old'Self{double_data = P'.mergeAppend (double_data old'Self) new'Field})
                    (P'.wireGetPacked 1)
             88 -> Prelude'.fmap (\ !new'Field -> old'Self{uint64_data = P'.append (uint64_data old'Self) new'Field}) (P'.wireGet 4)
             90 -> Prelude'.fmap (\ !new'Field -> old'Self{uint64_data = P'.mergeAppend (uint64_data old'Self) new'Field})
                    (P'.wireGetPacked 4)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TensorProto) TensorProto where
  getVal m' f' = f' m'

instance P'.GPB TensorProto

instance P'.ReflectDescriptor TensorProto where
  getMessageInfo _
   = P'.GetMessageInfo (P'.fromDistinctAscList [])
      (P'.fromDistinctAscList [8, 10, 16, 26, 34, 37, 40, 42, 50, 56, 58, 66, 74, 81, 82, 88, 90, 98, 106, 112])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorProto\"}, descFilePath = [\"Onnx\",\"TensorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.dims\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"dims\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Just (WireTag {getWireTag = 8},WireTag {getWireTag = 10}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.data_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"data_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.segment\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"segment\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto.Segment\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorProto\"], baseName = MName \"Segment\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.float_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"float_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Just (WireTag {getWireTag = 37},WireTag {getWireTag = 34}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 2}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.int32_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"int32_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 42}, packedTag = Just (WireTag {getWireTag = 40},WireTag {getWireTag = 42}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.string_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"string_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.int64_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"int64_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Just (WireTag {getWireTag = 56},WireTag {getWireTag = 58}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.doc_string\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"doc_string\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 12}, wireTag = WireTag {getWireTag = 98}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.raw_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"raw_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.external_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"external_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 13}, wireTag = WireTag {getWireTag = 106}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.StringStringEntryProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"StringStringEntryProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.data_location\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"data_location\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 14}, wireTag = WireTag {getWireTag = 112}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto.DataLocation\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorProto\"], baseName = MName \"DataLocation\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.double_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"double_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 82}, packedTag = Just (WireTag {getWireTag = 81},WireTag {getWireTag = 82}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorProto.uint64_data\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorProto\"], baseName' = FName \"uint64_data\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Just (WireTag {getWireTag = 88},WireTag {getWireTag = 90}), wireTagLength = 1, isPacked = True, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType TensorProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TensorProto where
  textPut msg
   = do
       P'.tellT "dims" (dims msg)
       P'.tellT "data_type" (data_type msg)
       P'.tellT "segment" (segment msg)
       P'.tellT "float_data" (float_data msg)
       P'.tellT "int32_data" (int32_data msg)
       P'.tellT "string_data" (string_data msg)
       P'.tellT "int64_data" (int64_data msg)
       P'.tellT "name" (name msg)
       P'.tellT "doc_string" (doc_string msg)
       P'.tellT "raw_data" (raw_data msg)
       P'.tellT "external_data" (external_data msg)
       P'.tellT "data_location" (data_location msg)
       P'.tellT "double_data" (double_data msg)
       P'.tellT "uint64_data" (uint64_data msg)
  textGet
   = do
       mods <- P'.sepEndBy
                (P'.choice
                  [parse'dims, parse'data_type, parse'segment, parse'float_data, parse'int32_data, parse'string_data,
                   parse'int64_data, parse'name, parse'doc_string, parse'raw_data, parse'external_data, parse'data_location,
                   parse'double_data, parse'uint64_data])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'dims
         = P'.try
            (do
               v <- P'.getT "dims"
               Prelude'.return (\ o -> o{dims = P'.append (dims o) v}))
        parse'data_type
         = P'.try
            (do
               v <- P'.getT "data_type"
               Prelude'.return (\ o -> o{data_type = v}))
        parse'segment
         = P'.try
            (do
               v <- P'.getT "segment"
               Prelude'.return (\ o -> o{segment = v}))
        parse'float_data
         = P'.try
            (do
               v <- P'.getT "float_data"
               Prelude'.return (\ o -> o{float_data = P'.append (float_data o) v}))
        parse'int32_data
         = P'.try
            (do
               v <- P'.getT "int32_data"
               Prelude'.return (\ o -> o{int32_data = P'.append (int32_data o) v}))
        parse'string_data
         = P'.try
            (do
               v <- P'.getT "string_data"
               Prelude'.return (\ o -> o{string_data = P'.append (string_data o) v}))
        parse'int64_data
         = P'.try
            (do
               v <- P'.getT "int64_data"
               Prelude'.return (\ o -> o{int64_data = P'.append (int64_data o) v}))
        parse'name
         = P'.try
            (do
               v <- P'.getT "name"
               Prelude'.return (\ o -> o{name = v}))
        parse'doc_string
         = P'.try
            (do
               v <- P'.getT "doc_string"
               Prelude'.return (\ o -> o{doc_string = v}))
        parse'raw_data
         = P'.try
            (do
               v <- P'.getT "raw_data"
               Prelude'.return (\ o -> o{raw_data = v}))
        parse'external_data
         = P'.try
            (do
               v <- P'.getT "external_data"
               Prelude'.return (\ o -> o{external_data = P'.append (external_data o) v}))
        parse'data_location
         = P'.try
            (do
               v <- P'.getT "data_location"
               Prelude'.return (\ o -> o{data_location = v}))
        parse'double_data
         = P'.try
            (do
               v <- P'.getT "double_data"
               Prelude'.return (\ o -> o{double_data = P'.append (double_data o) v}))
        parse'uint64_data
         = P'.try
            (do
               v <- P'.getT "uint64_data"
               Prelude'.return (\ o -> o{uint64_data = P'.append (uint64_data o) v}))