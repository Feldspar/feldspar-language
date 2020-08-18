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

import qualified Onnx.AttributeProto as A
import qualified Onnx.GraphProto as G
import qualified Onnx.ModelProto as O
import qualified Onnx.NodeProto as N
import qualified Onnx.TensorProto as TP
import qualified Onnx.TensorProto.DataType as TD
import qualified Onnx.TensorShapeProto as TSP
import qualified Onnx.TensorShapeProto.Dimension as TSP
import qualified Onnx.TensorShapeProto.Dimension.Value as TSP
import qualified Onnx.TypeProto as T
import qualified Onnx.TypeProto.Tensor as TT
import qualified Onnx.TypeProto.Value as TV
import qualified Onnx.ValueInfoProto as V

import Text.ProtocolBuffers (messageGet)
import Text.ProtocolBuffers.Header as H (Utf8(..))

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy.Builder as B

import qualified Data.Foldable as D (toList, foldMap)
import qualified Data.Set as S
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, (<.>))
import System.IO (IOMode(WriteMode), BufferMode(BlockBuffering), openFile, hClose, hPutStr
                 , hSetBuffering, hSetBinaryMode)

main :: IO ()
main = do args <- getArgs
          let [modelFileName] = take 1 args -- First argument is file name
              modelBaseName = takeBaseName modelFileName
              dataFileName = modelBaseName <.> "data"
              progFileName = modelBaseName <.> "hs"
          modelBinary <- L.readFile modelFileName
          let model = either (\s -> error $ "Error: " ++ s) fst $ messageGet modelBinary
              gr = fromMaybe (error "No graph in model") $ O.graph model

          -- Write the weights
          dfile <- openFile dataFileName WriteMode
          hSetBinaryMode dfile True
          hSetBuffering dfile $ BlockBuffering Nothing
          B.hPutBuilder dfile $ D.foldMap buildInitTensor $ G.initializer gr
          hClose dfile

          -- Write the program
          pfile <- openFile progFileName WriteMode
          hPutStr pfile $ mkProgramFile gr
          hClose pfile

-- | Extract initialized tensors
buildInitTensor :: TP.TensorProto -> B.Builder
buildInitTensor t = B.stringUtf8 (unwords $ showElemT dt : show (length ds) : map show ds)
                    <> B.string8 "\n"
                    <> buildValues dt t
  where ds = D.toList $ TP.dims t
        dt = int2elemT $ fromJust $ TP.data_type t


-- | Print the elements of a tensor
-- The TensorProto does not carry data for all element types
buildValues :: TD.DataType -> TP.TensorProto -> B.Builder
buildValues TD.FLOAT      t = D.foldMap (\ x -> B.floatDec x       <> B.string8 "\n") $ TP.float_data t
buildValues TD.DOUBLE     t = D.foldMap (\ x -> B.doubleDec x      <> B.string8 "\n") $ TP.double_data t
buildValues TD.INT32      t = D.foldMap (\ x -> B.int32Dec x       <> B.string8 "\n") $ TP.int32_data t
buildValues TD.UINT64     t = D.foldMap (\ x -> B.word64Dec x      <> B.string8 "\n") $ TP.uint64_data t
buildValues TD.INT64      t = D.foldMap (\ x -> B.int64Dec x       <> B.string8 "\n") $ TP.int64_data t
buildValues TD.STRING     t = D.foldMap (\ x -> B.lazyByteString x     <> B.string8 "\n") $ TP.string_data t
buildValues td            _ = error $ "onnxToFeld.buildValues: unsupported element type " ++ showElemT td

-- | Construct a Feldspar program corresponding to the ONNX graph
mkProgramFile :: G.GraphProto -> String
mkProgramFile gr 
  = unlines [ "module Main where"
            , ""
            , "import Feldspar"
            , "import Feldspar.Compiler (program)"
            , "import Feldspar.Onnx.Operators"
            , ""
            , "main = program " ++ name
            , ""
            , name ++ " " ++ unwords params ++ " = " ++ tuplify (map (mangle . vipName) $ D.toList $ G.output gr)
            , "  where "
              ++ intercalate "\n        " ("-- Nodes " : concatMap mkNode nodes)
            ]
  where name = "model" -- strFromJ $ G.name gr
        wName = "weights"
        initVs = map (toStr . fromJust . TP.name) $ D.toList $ G.initializer gr
        initSet = S.fromList initVs
        params = wName : map mkParam inps
        mkParam p = mangle $ vipName p
        inps = filter (\ p -> vipName p `S.notMember` initSet) $ D.toList $ G.input gr
        nodes = D.toList $ G.node gr

-- | Construct a Feldspar pattern binding for an ONNX graph node
mkNode :: N.NodeProto -> [String]
mkNode n = [outs ++ " = " ++ op ++ " " ++ attrs]
         ++ [ "    " ++ mangle (toStr v) | v <- D.toList $ N.input n]
         ++ [""]
  where outs = tuplify $ map (mangle . toStr) $ D.toList $ N.output n
        op = "onnx" ++ toStr (fromJust $ N.op_type n) ++ "_" ++ show (length $ D.toList $ N.input n)
        attrs = "[" ++ intercalate ", " (map showAttribute $ D.toList $ N.attribute n) ++ "]"

-- | Mngle an ONNX node mane to a Feldspar identifier
mangle :: String -> String
mangle s = "m_" ++ s

showAttribute :: A.AttributeProto -> String
showAttribute a = "(\"" ++ nStr ++ "\", " ++ val nStr ++ ")"
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

showAttrInts :: A.AttributeProto -> String
showAttrInts a = "AAInts " ++ listify (map show $ D.toList $ A.ints a)

showAttrInt :: A.AttributeProto -> String
showAttrInt a = "AAInt " ++ show (fromJust $ A.i a)

showAttrFloat :: A.AttributeProto -> String
showAttrFloat a = "AAFloat " ++ show (fromJust $ A.f a)

showTensorType :: (Integral a, Show b) => Maybe a -> [b] -> String
showTensorType t dims = "Data [" ++ t' ++ "]" ++ " -- " ++ unwords sh
  where t' = showElemT (int2elemT $ fromJust t)
        sh = map show dims

int2elemT :: Integral a => a -> TD.DataType
int2elemT i = toEnum $ fromIntegral i :: TD.DataType

showElemT :: TD.DataType -> String
showElemT TD.FLOAT16    = error $ "onnxToFeld.showElemT: FLOAT16 not implemented"
showElemT TD.BFLOAT16   = error $ "onnxToFeld.showElemT: BFLOAT16 not implemented"
showElemT TD.FLOAT      = "Float"
showElemT TD.DOUBLE     = "Double"
showElemT TD.UINT8      = "Word8"
showElemT TD.INT8       = "Int8"
showElemT TD.UINT16     = "Word16"
showElemT TD.INT16      = "Int16"
showElemT TD.UINT32     = "Word32"
showElemT TD.INT32      = "Int32"
showElemT TD.UINT64     = "Word64"
showElemT TD.INT64      = "Int64"
showElemT TD.COMPLEX64  = "(Complex Float)"
showElemT TD.COMPLEX128 = "(Complex Double)"
showElemT TD.STRING     = "String"
showElemT TD.BOOL       = "Bool"
showElemT TD.UNDEFINED  = error $ "onnxToFeld.showElemT: UNDEFINED not implemented"

showVI :: V.ValueInfoProto -> String
showVI v = nStr ++ " : " ++ tyStr
  where nStr = toStr $ fromJust $ V.name v
        tyStr = showType $ fromJust $ T.value $ fromJust $ V.type' v

showType :: TV.Value -> String
showType TV.Tensor_type{TV.tensor_type = t}
  = showTensorType (TT.elem_type t)
                   (map TSP.value $ D.toList $ TSP.dim $ fromJust $ TT.shape t)
showType TV.Sequence_type{} = "Sequence"
showType TV.Map_type{} = "Map"

showDim :: Maybe TSP.Value -> String
showDim (Just (TSP.Dim_value i)) = show i
showDim (Just (TSP.Dim_param p)) = toStr p
showDim Nothing = "*"

toStr :: H.Utf8 -> String
toStr (H.Utf8 s) = U.toString s

-- | Peel off a Just and convert to a String
strFromJ :: Maybe H.Utf8 -> String
strFromJ = toStr . fromJust

-- | Get the name of a ValueInfoProto
vipName :: V.ValueInfoProto -> String
vipName = strFromJ . V.name

-- | Make a tuple expression out of a non-singleton list
tuplify :: [String] -> String
tuplify [s] = s
tuplify ss  = "(" ++ intercalate ", " ss ++ ")"

-- | Make a list expression
listify :: [String] -> String
listify ss = "[" ++ intercalate ", " ss ++ "]"
