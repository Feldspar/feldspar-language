{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.SparseTensorProto (SparseTensorProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TensorProto as Onnx (TensorProto)

data SparseTensorProto = SparseTensorProto{values :: !(P'.Maybe Onnx.TensorProto), indices :: !(P'.Maybe Onnx.TensorProto),
                                           dims :: !(P'.Seq P'.Int64)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable SparseTensorProto where
  mergeAppend (SparseTensorProto x'1 x'2 x'3) (SparseTensorProto y'1 y'2 y'3)
   = SparseTensorProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)

instance P'.Default SparseTensorProto where
  defaultValue = SparseTensorProto P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire SparseTensorProto where
  wireSize ft' self'@(SparseTensorProto x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeRep 1 3 x'3)
  wirePutWithSize ft' self'@(SparseTensorProto x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize [P'.wirePutOptWithSize 10 11 x'1, P'.wirePutOptWithSize 18 11 x'2, P'.wirePutRepWithSize 24 3 x'3]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{values = P'.mergeAppend (values old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{indices = P'.mergeAppend (indices old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{dims = P'.append (dims old'Self) new'Field}) (P'.wireGet 3)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{dims = P'.mergeAppend (dims old'Self) new'Field}) (P'.wireGetPacked 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> SparseTensorProto) SparseTensorProto where
  getVal m' f' = f' m'

instance P'.GPB SparseTensorProto

instance P'.ReflectDescriptor SparseTensorProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 24, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.SparseTensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"SparseTensorProto\"}, descFilePath = [\"Onnx\",\"SparseTensorProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.SparseTensorProto.values\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"SparseTensorProto\"], baseName' = FName \"values\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.SparseTensorProto.indices\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"SparseTensorProto\"], baseName' = FName \"indices\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.SparseTensorProto.dims\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"SparseTensorProto\"], baseName' = FName \"dims\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Just (WireTag {getWireTag = 24},WireTag {getWireTag = 26}), wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = True, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType SparseTensorProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg SparseTensorProto where
  textPut msg
   = do
       P'.tellT "values" (values msg)
       P'.tellT "indices" (indices msg)
       P'.tellT "dims" (dims msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'values, parse'indices, parse'dims]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'values
         = P'.try
            (do
               v <- P'.getT "values"
               Prelude'.return (\ o -> o{values = v}))
        parse'indices
         = P'.try
            (do
               v <- P'.getT "indices"
               Prelude'.return (\ o -> o{indices = v}))
        parse'dims
         = P'.try
            (do
               v <- P'.getT "dims"
               Prelude'.return (\ o -> o{dims = P'.append (dims o) v}))