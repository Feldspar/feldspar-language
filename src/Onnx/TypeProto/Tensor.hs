{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.TypeProto.Tensor (Tensor(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TensorShapeProto as Onnx (TensorShapeProto)

data Tensor = Tensor{elem_type :: !(P'.Maybe P'.Int32), shape :: !(P'.Maybe Onnx.TensorShapeProto)}
              deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Tensor where
  mergeAppend (Tensor x'1 x'2) (Tensor y'1 y'2) = Tensor (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default Tensor where
  defaultValue = Tensor P'.defaultValue P'.defaultValue

instance P'.Wire Tensor where
  wireSize ft' self'@(Tensor x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1 + P'.wireSizeOpt 1 11 x'2)
  wirePutWithSize ft' self'@(Tensor x'1 x'2)
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
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{elem_type = Prelude'.Just new'Field}) (P'.wireGet 5)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{shape = P'.mergeAppend (shape old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Tensor) Tensor where
  getVal m' f' = f' m'

instance P'.GPB Tensor

instance P'.ReflectDescriptor Tensor where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TypeProto.Tensor\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Tensor\"}, descFilePath = [\"Onnx\",\"TypeProto\",\"Tensor.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.Tensor.elem_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Tensor\"], baseName' = FName \"elem_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.Tensor.shape\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Tensor\"], baseName' = FName \"shape\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorShapeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorShapeProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Tensor where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Tensor where
  textPut msg
   = do
       P'.tellT "elem_type" (elem_type msg)
       P'.tellT "shape" (shape msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'elem_type, parse'shape]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'elem_type
         = P'.try
            (do
               v <- P'.getT "elem_type"
               Prelude'.return (\ o -> o{elem_type = v}))
        parse'shape
         = P'.try
            (do
               v <- P'.getT "shape"
               Prelude'.return (\ o -> o{shape = v}))