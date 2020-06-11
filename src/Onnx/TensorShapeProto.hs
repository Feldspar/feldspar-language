{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorShapeProto (TensorShapeProto(..)) where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TensorShapeProto.Dimension as Onnx.TensorShapeProto (Dimension)

data TensorShapeProto = TensorShapeProto{dim :: !(P'.Seq Onnx.TensorShapeProto.Dimension)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable TensorShapeProto where
  mergeAppend (TensorShapeProto x'1) (TensorShapeProto y'1) = TensorShapeProto (P'.mergeAppend x'1 y'1)

instance P'.Default TensorShapeProto where
  defaultValue = TensorShapeProto P'.defaultValue

instance P'.Wire TensorShapeProto where
  wireSize ft' self'@(TensorShapeProto x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePutWithSize ft' self'@(TensorShapeProto x'1)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutRepWithSize 10 11 x'1]
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{dim = P'.append (dim old'Self) new'Field}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> TensorShapeProto) TensorShapeProto where
  getVal m' f' = f' m'

instance P'.GPB TensorShapeProto

instance P'.ReflectDescriptor TensorShapeProto where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".onnx.TensorShapeProto\", haskellPrefix = [], parentModule = [MName \"Onnx\"], baseName = MName \"TensorShapeProto\"}, descFilePath = [\"Onnx\",\"TensorShapeProto.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".onnx.TensorShapeProto.dim\", haskellPrefix' = [], parentModule' = [MName \"Onnx\",MName \"TensorShapeProto\"], baseName' = FName \"dim\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".onnx.TensorShapeProto.Dimension\", haskellPrefix = [], parentModule = [MName \"Onnx\",MName \"TensorShapeProto\"], baseName = MName \"Dimension\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False, jsonInstances = False}"

instance P'.TextType TensorShapeProto where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg TensorShapeProto where
  textPut msg
   = do
       P'.tellT "dim" (dim msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'dim]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'dim
         = P'.try
            (do
               v <- P'.getT "dim"
               Prelude'.return (\ o -> o{dim = P'.append (dim o) v}))