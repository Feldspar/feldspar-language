{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorAnnotation (TensorAnnotation(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.StringStringEntryProto as Onnx (StringStringEntryProto)

data TensorAnnotation = TensorAnnotation{tensor_name :: !(P'.Maybe P'.Utf8),
                                         quant_parameter_tensor_names :: !(P'.Seq Onnx.StringStringEntryProto)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TensorAnnotation where
  mergeAppend (TensorAnnotation x'1 x'2) (TensorAnnotation y'1 y'2)
   = TensorAnnotation (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default TensorAnnotation where
  defaultValue = TensorAnnotation P'.defaultValue P'.defaultValue

instance P'.Wire TensorAnnotation where
  wireSize ft' self'@(TensorAnnotation x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 9 x'1 + P'.wireSizeRep 1 11 x'2)
  wirePutWithSize ft' self'@(TensorAnnotation x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutOptWithSize 10 9 x'1, P'.wirePutRepWithSize 18 11 x'2]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{tensor_name = Prelude'.Just new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{quant_parameter_tensor_names = P'.append (quant_parameter_tensor_names old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TensorAnnotation) TensorAnnotation where
  getVal m' f' = f' m'

instance P'.GPB TensorAnnotation

instance P'.ReflectDescriptor TensorAnnotation where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TensorAnnotation\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorAnnotation\"}, descFilePath = [\"Onnx\",\"TensorAnnotation.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorAnnotation.tensor_name\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorAnnotation\"], baseName' = FName \"tensor_name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorAnnotation.quant_parameter_tensor_names\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorAnnotation\"], baseName' = FName \"quant_parameter_tensor_names\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.StringStringEntryProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"StringStringEntryProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType TensorAnnotation where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TensorAnnotation where
  textPut msg
   = do
       P'.tellT "tensor_name" (tensor_name msg)
       P'.tellT "quant_parameter_tensor_names" (quant_parameter_tensor_names msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'tensor_name, parse'quant_parameter_tensor_names]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'tensor_name
         = P'.try
            (do
               v <- P'.getT "tensor_name"
               Prelude'.return (\ o -> o{tensor_name = v}))
        parse'quant_parameter_tensor_names
         = P'.try
            (do
               v <- P'.getT "quant_parameter_tensor_names"
               Prelude'.return (\ o -> o{quant_parameter_tensor_names = P'.append (quant_parameter_tensor_names o) v}))