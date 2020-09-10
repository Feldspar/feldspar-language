{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.TrainingInfoProto (TrainingInfoProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.GraphProto as Onnx (GraphProto)
import qualified Onnx.StringStringEntryProto as Onnx (StringStringEntryProto)

data TrainingInfoProto = TrainingInfoProto{initialization :: !(P'.Maybe Onnx.GraphProto), algorithm :: !(P'.Maybe Onnx.GraphProto),
                                           initialization_binding :: !(P'.Seq Onnx.StringStringEntryProto),
                                           update_binding :: !(P'.Seq Onnx.StringStringEntryProto)}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TrainingInfoProto where
  mergeAppend (TrainingInfoProto x'1 x'2 x'3 x'4) (TrainingInfoProto y'1 y'2 y'3 y'4)
   = TrainingInfoProto (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)

instance P'.Default TrainingInfoProto where
  defaultValue = TrainingInfoProto P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue

instance P'.Wire TrainingInfoProto where
  wireSize ft' self'@(TrainingInfoProto x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeRep 1 11 x'4)
  wirePutWithSize ft' self'@(TrainingInfoProto x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = P'.sequencePutWithSize
            [P'.wirePutOptWithSize 10 11 x'1, P'.wirePutOptWithSize 18 11 x'2, P'.wirePutRepWithSize 26 11 x'3,
             P'.wirePutRepWithSize 34 11 x'4]
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
                    (\ !new'Field -> old'Self{initialization = P'.mergeAppend (initialization old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{algorithm = P'.mergeAppend (algorithm old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{initialization_binding = P'.append (initialization_binding old'Self) new'Field})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{update_binding = P'.append (update_binding old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TrainingInfoProto) TrainingInfoProto where
  getVal m' f' = f' m'

instance P'.GPB TrainingInfoProto

instance P'.ReflectDescriptor TrainingInfoProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TrainingInfoProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TrainingInfoProto\"}, descFilePath = [\"Onnx\",\"TrainingInfoProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TrainingInfoProto.initialization\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TrainingInfoProto\"], baseName' = FName \"initialization\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.GraphProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"GraphProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TrainingInfoProto.algorithm\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TrainingInfoProto\"], baseName' = FName \"algorithm\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.GraphProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"GraphProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TrainingInfoProto.initialization_binding\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TrainingInfoProto\"], baseName' = FName \"initialization_binding\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.StringStringEntryProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"StringStringEntryProto\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TrainingInfoProto.update_binding\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TrainingInfoProto\"], baseName' = FName \"update_binding\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.StringStringEntryProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"StringStringEntryProto\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType TrainingInfoProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TrainingInfoProto where
  textPut msg
   = do
       P'.tellT "initialization" (initialization msg)
       P'.tellT "algorithm" (algorithm msg)
       P'.tellT "initialization_binding" (initialization_binding msg)
       P'.tellT "update_binding" (update_binding msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'initialization, parse'algorithm, parse'initialization_binding, parse'update_binding])
                P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'initialization
         = P'.try
            (do
               v <- P'.getT "initialization"
               Prelude'.return (\ o -> o{initialization = v}))
        parse'algorithm
         = P'.try
            (do
               v <- P'.getT "algorithm"
               Prelude'.return (\ o -> o{algorithm = v}))
        parse'initialization_binding
         = P'.try
            (do
               v <- P'.getT "initialization_binding"
               Prelude'.return (\ o -> o{initialization_binding = P'.append (initialization_binding o) v}))
        parse'update_binding
         = P'.try
            (do
               v <- P'.getT "update_binding"
               Prelude'.return (\ o -> o{update_binding = P'.append (update_binding o) v}))