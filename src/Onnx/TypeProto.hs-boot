{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.TypeProto (TypeProto) where
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data TypeProto deriving Prelude'.Typeable

instance P'.MessageAPI msg' (msg' -> TypeProto) TypeProto

instance Prelude'.Show TypeProto

instance Prelude'.Eq TypeProto

instance Prelude'.Ord TypeProto

instance Prelude'.Data TypeProto

instance Prelude'.Generic TypeProto

instance P'.Mergeable TypeProto

instance P'.Default TypeProto

instance P'.Wire TypeProto

instance P'.GPB TypeProto

instance P'.ReflectDescriptor TypeProto

instance P'.TextType TypeProto

instance P'.TextMsg TypeProto