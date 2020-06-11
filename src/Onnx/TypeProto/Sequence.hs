{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TypeProto.Sequence (Sequence(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import {-# SOURCE #-} qualified Onnx.TypeProto as Onnx (TypeProto)

data Sequence = Sequence{elem_type :: !(P'.Maybe Onnx.TypeProto)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable Sequence where
  mergeAppend (Sequence x'1) (Sequence y'1) = Sequence (P'.mergeAppend x'1 y'1)

instance P'.Default Sequence where
  defaultValue = Sequence P'.defaultValue

instance P'.Wire Sequence where
  wireSize ft' self'@(Sequence x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1)
  wirePutWithSize ft' self'@(Sequence x'1)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutOptWithSize 10 11 x'1]
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
             10 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{elem_type = P'.mergeAppend (elem_type old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> Sequence) Sequence where
  getVal m' f' = f' m'

instance P'.GPB Sequence

instance P'.ReflectDescriptor Sequence where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TypeProto.Sequence\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TypeProto\"], baseName = MName \"Sequence\"}, descFilePath = [\"Onnx\",\"TypeProto\",\"Sequence.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TypeProto.Sequence.elem_type\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TypeProto\",MName \"Sequence\"], baseName' = FName \"elem_type\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TypeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TypeProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType Sequence where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg Sequence where
  textPut msg
   = do
       P'.tellT "elem_type" (elem_type msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'elem_type]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'elem_type
         = P'.try
            (do
               v <- P'.getT "elem_type"
               Prelude'.return (\ o -> o{elem_type = v}))