{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.TypeProto.Value where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Onnx.TypeProto.Map as Onnx.TypeProto (Map)
import qualified Onnx.TypeProto.Sequence as Onnx.TypeProto (Sequence)
import qualified Onnx.TypeProto.Tensor as Onnx.TypeProto (Tensor)

data Value = Tensor_type{tensor_type :: (Onnx.TypeProto.Tensor)}
           | Sequence_type{sequence_type :: (Onnx.TypeProto.Sequence)}
           | Map_type{map_type :: (Onnx.TypeProto.Map)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)
get'tensor_type x
 = case x of
     Tensor_type tensor_type -> Prelude'.Just tensor_type
     _ -> Prelude'.Nothing
get'sequence_type x
 = case x of
     Sequence_type sequence_type -> Prelude'.Just sequence_type
     _ -> Prelude'.Nothing
get'map_type x
 = case x of
     Map_type map_type -> Prelude'.Just map_type
     _ -> Prelude'.Nothing

instance P'.Default Value where
  defaultValue = Tensor_type P'.defaultValue

instance P'.Mergeable Value