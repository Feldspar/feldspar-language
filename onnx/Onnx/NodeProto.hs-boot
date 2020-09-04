{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# OPTIONS_GHC  -w #-}
module Onnx.NodeProto (NodeProto) where
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data NodeProto deriving Prelude'.Typeable

instance P'.MessageAPI msg' (msg' -> NodeProto) NodeProto

instance Prelude'.Show NodeProto

instance Prelude'.Eq NodeProto

instance Prelude'.Ord NodeProto

instance Prelude'.Data NodeProto

instance Prelude'.Generic NodeProto

instance P'.Mergeable NodeProto

instance P'.Default NodeProto

instance P'.Wire NodeProto

instance P'.GPB NodeProto

instance P'.ReflectDescriptor NodeProto

instance P'.TextType NodeProto

instance P'.TextMsg NodeProto