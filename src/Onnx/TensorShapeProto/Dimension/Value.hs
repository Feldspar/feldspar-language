{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Onnx.TensorShapeProto.Dimension.Value where
import Prelude ((+), (/), (++), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data Value = Dim_value{dim_value :: (P'.Int64)}
           | Dim_param{dim_param :: (P'.Utf8)}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)
get'dim_value x
 = case x of
     Dim_value dim_value -> Prelude'.Just dim_value
     _ -> Prelude'.Nothing
get'dim_param x
 = case x of
     Dim_param dim_param -> Prelude'.Just dim_param
     _ -> Prelude'.Nothing

instance P'.Default Value where
  defaultValue = Dim_value P'.defaultValue

instance P'.Mergeable Value