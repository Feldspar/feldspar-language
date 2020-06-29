--
-- Copyright (c) 2020, ERICSSON AB
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright notice,
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
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
import Text.ProtocolBuffers.Header as H (Utf8(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U

import Data.Either (either)
import qualified Data.Foldable as D (toList)
import Data.List (intercalate)
import Data.Maybe (maybe, fromJust, fromMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do args <- getArgs
          let [modelFileName] = take 1 args -- First argument is file name
          -- putStrLn modelFileName
          modelBinary <- L.readFile modelFileName
          -- putStrLn "Read it!"
          let model = either (\s -> error $ "Error: " ++ s) fst $ messageGet modelBinary
              gr = fromMaybe (error "No graph in model") $ graph model
          -- putStrLn $ maybe (error "Graph has no name") toStr $ G.name gr
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

showNode :: NodeProto -> String
showNode n = "  " ++ outs ++ " = " ++ op ++ " " ++ ins ++ "\n" ++ attrs
  where ins = unwords $ map toStr $ D.toList $ N.input n
        -- nameStr = maybe (error "No name string") toStr $ N.name n
        op = "onnx" ++ toStr (fromJust $ N.op_type n)
        outs = unwords $ map toStr $ D.toList $ N.output n
        attrs = unlines $ map showAttribute $ D.toList $ N.attribute n

showAttribute :: AttributeProto -> String
showAttribute a = "  -- " ++ nStr ++ " " ++ val nStr
  where nStr = toStr (fromJust $ A.name a)
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

showAttrInts :: AttributeProto -> String
showAttrInts  a = unwords $ map show $ D.toList $ ints a

showAttrInt :: AttributeProto -> String
showAttrInt a = show $ fromJust $ i a

showAttrFloat :: AttributeProto -> String
showAttrFloat a = show $ fromJust $ f a

showGI :: TensorProto -> String
showGI t = toStr (fromJust $ TP.name t) ++ " :: "
           ++ showTensorType (TP.data_type t) (D.toList $ TP.dims t) ++ "\n"
           ++ toStr (fromJust $ TP.name t) ++ " = value [" ++ elems ++ "]"
  where elems = intercalate ", " (map show $ D.toList $ TP.float_data t)

showGIc :: TensorProto -> String
showGIc t | length (D.toList $ TP.float_data t) < 100000
          = showElemT (int2elemT $ fromJust $ TP.data_type t) ++ " " ++ toStr (fromJust $ TP.name t) ++ "[] = {"
            ++ intercalate ", " (map show $ D.toList $ TP.float_data t) ++ "};"
          | otherwise
          = "// omitted "

showTensorType :: (Integral a, Show b) => Maybe a -> [b] -> String
showTensorType t dims = "Data [" ++ t' ++ "]" ++ " -- " ++ unwords sh
  where t' = showElemT (int2elemT $ fromJust t)
        sh = map show dims

int2elemT :: Integral a => a -> DataType
int2elemT i = toEnum $ fromIntegral i :: TD.DataType

showElemT :: DataType -> String
showElemT FLOAT = "Float"
showElemT t = error $ "Type " ++ show t ++ " not implemented"

showVI :: ValueInfoProto -> String
showVI v = nStr ++ " : " ++ tyStr
  where nStr = toStr $ fromJust $ V.name v
        tyStr = showType $ fromJust $ T.value $ fromJust $ V.type' v

showType :: TV.Value -> String
showType Tensor_type{TV.tensor_type = t}
  = showTensorType (TT.elem_type t)
                   (map TSP.value $ D.toList $ TSP.dim $ fromJust $ TT.shape t)
showType Sequence_type{} = "Sequence"
showType Map_type{} = "Map"

showDim :: Maybe TSP.Value -> String
showDim (Just (TSP.Dim_value i)) = show i
showDim (Just (TSP.Dim_param p)) = toStr p
showDim Nothing = "*"

toStr :: H.Utf8 -> String
toStr (H.Utf8 s) = U.toString s
