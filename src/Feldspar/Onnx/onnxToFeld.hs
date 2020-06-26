{-# OPTIONS_GHC -Wall #-}

module Main where

import Onnx.ModelProto
import Onnx.GraphProto as G
import Onnx.NodeProto as N
import Onnx.ValueInfoProto as V
import Onnx.AttributeProto as A
import Onnx.TypeProto as T
import Onnx.TypeProto.Value as TV
import Onnx.TypeProto.Tensor as TT
import Onnx.TypeProto.Sequence as TS
import Onnx.TypeProto.Map as TM
import Onnx.TensorProto as TP
import Onnx.TensorProto.DataType as TD
import Onnx.TensorShapeProto as TSP
import Onnx.TensorShapeProto.Dimension as TSP
import Onnx.TensorShapeProto.Dimension.Value as TSP

import Text.ProtocolBuffers
import Text.ProtocolBuffers.Header as H(Utf8(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U

import qualified Data.Foldable as D(toList)
import Data.List (intercalate)

import System.Environment(getArgs)

main = do args <- getArgs
          let [modelFileName] = take 1 args -- First argument is file name
          -- putStrLn modelFileName
          modelBinary <- L.readFile modelFileName
          -- putStrLn "Read it!"
          let model = case messageGet modelBinary of
                         Left msg -> error $ "Error: " ++ msg
                         Right (m,_) -> m
          let gr = case graph model of
                     Nothing -> error $ "No graph in model"
                     Just g -> g
          let grName = case G.name gr of
                         Nothing -> error "This graph has no name"
                         Just (H.Utf8 n) -> n
          -- putStrLn $ U.toString grName
          let graphNodes = D.toList $ node gr
          -- putStrLn $ show $ length graphNodes
          -- putStrLn "module Inits where"
          -- putStrLn "import Feldspar"
          -- putStr   "-- Num graph initializers "
          -- putStrLn $ show $ length $ D.toList $ G.initializer gr
          -- putStrLn "-- Graph initializers"
          -- putStrLn "typedef float Float;"
          -- putStrLn $ unlines $ map showGIc $ take 1000 $ D.toList $ G.initializer gr
          putStrLn "Graph inputs"
          putStrLn $ unlines $ map showVI $ D.toList $ G.input gr
          putStrLn "Graph nodes"
          putStr $ unlines $ map showNode graphNodes

showNode n = "  " ++ outs ++ " = " ++ op ++ " " ++ ins ++ "\n" ++ attrs
  where ins = unwords $ map toStr $ D.toList $ N.input n
        nameStr = toStr $ unJust $ N.name n
        op = "onnx" ++ (toStr $ unJust $ N.op_type n)
        outs = unwords $ map toStr $ D.toList $ N.output n
        attrs = unlines $ map showAttribute $ D.toList $ N.attribute n

showAttribute a = "  -- " ++ nStr ++ " " ++ val nStr
  where nStr = toStr (unJust $ A.name a)
        val "dilations"    = showAttrInts a
        val "group"        = showAttrInt a
        val "kernel_shape" = showAttrInts a
        val "pads"         = showAttrInts a
        val "strides"      = showAttrInts a
        -- BN
        val "epsilon"      = showAttrFloat a
        val "momentum"     = showAttrFloat a
        val "spatial"      = showAttrInt a
        -- Gemm
        val "alpha"        = showAttrFloat a
        val "beta"         = showAttrFloat a
        val "transA"       = showAttrInt a
        val "transB"       = showAttrInt a
        val _ = "_|_"

showAttrInts  a = intercalate " " $ map show $ D.toList $ ints a
showAttrInt   a = show $ unJust $ i a
showAttrFloat a = show $ unJust $ f a

showGI t = toStr (unJust $ TP.name t) ++ " :: " ++ showTensorTypeI (TP.data_type t) (D.toList $ TP.dims t)
        ++ "\n" ++ toStr (unJust $ TP.name t) ++ " = value [" 
            ++ intercalate ", " (map show $ D.toList $ TP.float_data t) ++ "]"

showGIc t | (length $ D.toList $ TP.float_data t) < 100000
          = showElemT (int2elemT $ unJust $ TP.data_type t) ++ " " ++ toStr (unJust $ TP.name t) ++ "[] = {"
            ++ intercalate ", " (map show $ D.toList $ TP.float_data t) ++ "};"
          | otherwise
          = "// omitted "

-- showTensorType t dims = "Tensor [" ++ unwords sh ++ "] of " ++ show (toEnum $ fromIntegral $ unJust t :: TD.DataType)
--   where sh = map show dims

showTensorType t dims = "Data [" ++ showElemT (int2elemT $ unJust t) ++ "]" ++ " -- " ++ intercalate " " sh
  where sh = map showDim dims

showTensorTypeI t dims = "Data [" ++ showElemT (int2elemT $ unJust t) ++ "]" ++ " -- " ++ intercalate " " sh
  where sh = map show dims

-- showDim (Just (Dim_value i)) = show i
-- showDim (Just (Dim_param s)) = toStr s
-- showDim _ = "*"

int2elemT i = toEnum $ fromIntegral i :: TD.DataType

showElemT FLOAT = "Float"
showElemT t = error $ "Type " ++ show t ++ " not implemented"

showVI v = nStr ++ " : " ++ tyStr
  where nStr = toStr $ unJust $ V.name v
        tyStr = showType $ unJust $ T.value $ unJust $ V.type' v

showType Tensor_type{TV.tensor_type = t}
   = showTensorType (TT.elem_type t) (map TSP.value $ D.toList $ TSP.dim $ unJust $ TT.shape t)
showType Sequence_type{} = "Sequence"
showType Map_type{} = "Map"

showDim (Just (TSP.Dim_value i)) = show i
showDim (Just (TSP.Dim_param p)) = toStr p
showDim Nothing = "*"

unJust (Just x) = x
unJust Nothing = error $ "unJust on Nothing"

toStr :: H.Utf8 -> String
toStr (H.Utf8 s) = U.toString s
