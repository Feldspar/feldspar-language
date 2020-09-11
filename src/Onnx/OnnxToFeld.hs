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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (show)
import qualified Prelude as P

import Feldspar.Compiler.Imperative.Frontend (mkStructType, mkAwLType)
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Lattice (universal)

import qualified Onnx.AttributeProto as A
import qualified Onnx.GraphProto as G
import qualified Onnx.ModelProto as O
import qualified Onnx.NodeProto as N
import qualified Onnx.TensorProto as TP
import qualified Onnx.TensorProto.DataType as TD
import qualified Onnx.TensorShapeProto as TSP
import qualified Onnx.TensorShapeProto.Dimension as TSP
import qualified Onnx.TensorShapeProto.Dimension.Value as TSP
import qualified Onnx.TypeProto.Tensor as TT
import qualified Onnx.TypeProto.Value as TV
import qualified Onnx.ValueInfoProto as V

import Text.ProtocolBuffers (Utf8, Int64, messageGet, utf8)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy.Builder as B

import qualified Data.Foldable as D (toList, foldMap, length, concatMap)
import qualified Data.Sequence as D (Seq, sortBy, sort)
import qualified Data.Set as S
import Data.List (groupBy, group)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
import Data.String (IsString(..))
import System.Environment (getArgs)
import System.FilePath (takeBaseName, (<.>))
import System.IO (IOMode(WriteMode), BufferMode(BlockBuffering), openFile, hClose
                 , hSetBuffering, hSetBinaryMode)
import Text.FShow.RealFloat (fshow)

data TensorInfo = TI { tiField :: Int         -- ^ The zero based index of the group the tensor is part of.
                     , tiIdx   :: Int         -- ^ The zero based index of the tensor within its group.
                     , tiType  :: TD.DataType -- ^ The element type of the tensor.
                     , tiDims  :: D.Seq Int64 -- ^ The size of the tensor in each dimension (the length is the dimensionality).
                     , tiName  :: Utf8        -- ^ The name of the tensor
                     }

main :: IO ()
main = do args <- getArgs
          let [modelFileName] = take 1 args -- First argument is file name
              modelBaseName = takeBaseName modelFileName
              dataFileName = modelBaseName <.> "data"
              progFileName = modelBaseName <.> "hs"
              initFileName = modelBaseName <> "-init" <.> "c"
              cHdrFileName = modelBaseName <.> "h"
          modelBinary <- L.readFile modelFileName
          let model = either (\s -> error $ "Error: " ++ s) fst $ messageGet modelBinary
              gr = fromMaybe (error "No graph in model") $ O.graph model
              proj t = (TP.data_type t, D.length $ TP.dims t)
              ordInits = D.sortBy (comparing proj) $ G.initializer gr
              initGroups = groupBy (\ x y -> proj x == proj y) $ D.toList ordInits
              iTensorInfo = [ [TI f i ty (TP.dims tp) (fromJust $ TP.name tp)
                               | (tp,i) <- zip tps [0 ..]]
                             | (tps,f) <- zip initGroups [0 ..],
                               let ty = int2elemT $ TP.data_type $ head tps]
              allUses = group $ D.toList $ D.sort $ foldMap N.input $ G.node gr
              multiUses = S.fromList $ map head $ filter ((>1) . length) allUses

          -- Write the weights
          dfile <- openFile dataFileName WriteMode
          hSetBinaryMode dfile True
          hSetBuffering dfile $ BlockBuffering Nothing
          B.hPutBuilder dfile $ D.foldMap buildInitTensor ordInits
          hClose dfile

          -- Write the program
          pfile <- openFile progFileName WriteMode
          L.hPutStr pfile $ mkProgramFile gr iTensorInfo multiUses
          hClose pfile

          -- Write the init module
          mfile <- openFile initFileName WriteMode
          L.hPutStr mfile $ mkInitReadFile cHdrFileName iTensorInfo
          hClose mfile

-- | Extract initialized tensors
buildInitTensor :: TP.TensorProto -> B.Builder
buildInitTensor t = B.lazyByteString (L.unwords $ showElemT dt : show (length ds) : map show ds)
                    <> B.string8 "\n"
                    <> buildValues dt t
  where ds = D.toList $ TP.dims t
        dt = int2elemT $ TP.data_type t


-- | Print the elements of a tensor
-- This function handles only those element types that have specialized
-- representation for initialization in the ONNX specification, other types
-- are packed into raw_data.
buildValues :: TD.DataType -> TP.TensorProto -> B.Builder
buildValues TD.FLOAT =
  D.foldMap (\ x -> (B.string7 . fshow $ x) <> B.string8 "\n") . TP.float_data
buildValues TD.DOUBLE =
  D.foldMap (\ x -> (B.string7 . fshow $ x) <> B.string8 "\n") . TP.double_data
buildValues TD.INT32 =
  D.foldMap (\ x -> B.int32Dec x            <> B.string8 "\n") . TP.int32_data
buildValues TD.UINT64 =
  D.foldMap (\ x -> B.word64Dec x           <> B.string8 "\n") . TP.uint64_data
buildValues TD.INT64 =
  D.foldMap (\ x -> B.int64Dec x            <> B.string8 "\n") . TP.int64_data
buildValues TD.STRING =
  D.foldMap (\ x -> B.lazyByteString x      <> B.string8 "\n") . TP.string_data
buildValues td =
  error $ "onnxToFeld.buildValues: unsupported element type " ++ U.toString (showElemT td)

-- | Construct a Feldspar program corresponding to the ONNX graph
mkProgramFile :: G.GraphProto -> [[TensorInfo]] -> S.Set Utf8 -> L.ByteString
mkProgramFile gr initGroups multiUses
  = L.unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE TypeOperators #-}"
            , "{-# LANGUAGE FlexibleContexts #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , "{-# LANGUAGE ScopedTypeVariables #-}"
            , ""
            , "module Main where"
            , ""
            , "import Feldspar"
            , "import Feldspar.Compiler (program)"
            , "import Feldspar.Core.NestedTuples"
            , "import Feldspar.Onnx.Operators"
            , "import Feldspar.Vector"
            , "import GHC.TypeLits"
            , ""
            , "type WeightRec"
            , "  = Tuple"
            , "    (" <> L.concat [mkITH ti <> " ':\n     " | ti:_ <- initGroups] <> "'[]\n    )"
            , ""
            , "main = program " <> name
            , ""
            , name <> " " <> L.unwords params <> " = " <> tuplify (map (mangle . vipName) $ D.toList $ G.output gr)
            , "  where "
              <> L.intercalate "\n        " (accesses ++ "-- Nodes" : D.concatMap (mkNode multiUses) (G.node gr))
            ]
  where name = mangle $ fromJust $ G.name gr
        initSet = S.fromList $ map tiName $ concat initGroups
        params = "(weights :: WeightRec)" : map (mangle . vipName) inps
        inps = filter (\ p -> vipName p `S.notMember` initSet) $ D.toList $ G.input gr
        accesses = map mkAccess $ concat initGroups

-- | Compute a string representation of the Haskell type of a group of initialized tensors
--   with the same dimensionality and element type.
mkITH :: TensorInfo -> L.ByteString
mkITH ti = "Pull DIM1 (DPull (" <> mkSh (D.toList $ tiDims ti) <> ") " <> eTyH <>")"
  where mkSh ds = "Z :. " <> L.intercalate " :. " ["Data Length" | _ <- ds]
        eTyH = showElemT $ tiType ti

-- | Construct a Feldspar pattern binding for an ONNX graph node
mkNode :: S.Set Utf8 -> N.NodeProto -> [L.ByteString]
mkNode mUses n = [outs <> " = " <> forcing <> op <> " " <> attrs]
              ++ [ "    " <> mangle v | v <- D.toList $ N.input n]
              ++ [""]
  where outs = tuplify $ map mangle $ D.toList $ N.output n
        op = "onnx" <> strFromJ (N.op_type n) <> "_" <> show (D.length $ N.input n)
        attrs = "[" <> L.intercalate ", " (map showAttribute $ D.toList $ N.attribute n) <> "]"
        forcing = if any (flip S.member mUses) $ N.output n then "force $ " else ""

-- | Read from the weight record
mkAccess :: TensorInfo -> L.ByteString
mkAccess ti = vname <> " = sel (Proxy @" <> show (tiField ti) <> ") weights ! (Z :. " <> show (tiIdx ti) <> ")"
  where vname = mangle $ tiName ti

-- | Constuct the C code to read initialized tensors
mkInitReadFile :: FilePath -> [[TensorInfo]] -> L.ByteString
mkInitReadFile hf initGroups = L.concat $ start : map initVec initGroups ++ map initTensor (concat initGroups) ++ [end]
  where start = L.unlines
                [ "#include \"" <> fromString hf <> "\""
                , ""
                , "#include <stdlib.h>"
                , "#include <stdio.h>"
                , "#include <inttypes.h>"
                , ""
                , weightRecTC <> " * "
                , "read_constants( char* wfile_name )"
                , "{"
                , "  FILE* wfile = fopen( wfile_name, \"r\" );"
                , "  " <> weightRecTC <> " *"
                , "    w = malloc( sizeof(" <> weightRecTC <> ") );"
                , ""
                ]
        end = L.unlines
                [ ""
                , "  fclose( wfile );"
                , "  return 0;"
                , "}"
                ]
        initVec (ti:tis) = "  w->member" <> field <> ".length = " <> show (length tis + 1) <> ";\n"
                         <> "  w->member" <> field <> ".buffer = malloc( sizeof(" <> t <> ") * "
                               <> "w->member" <> field <> ".length );\n"
            where t = fromString $ renderType $ tiToType ti
                  field = show (tiField ti + 1)
        initVec [] = error "Impossible"
        weightRecTC = fromString $ renderType $ mkStructType $ map toFieldType initGroups
        toFieldType (ti:_) = ("member" ++ show (tiField ti + 1), mkAwLType universal $ tiToType ti)
        toFieldType []     = error "Impossible"

-- | Compute the (Program) Type that corresponds to a tensor
tiToType :: TensorInfo -> Type
tiToType ti = goDim (D.length $ tiDims ti) (toType $ tiType ti)
  where goDim 0 _  = error "Zero dimensional tensor not implemented"
        goDim 1 tt = mkAwLType universal tt
        goDim n tt = mkStructType
                       [("member1", mkAwLType universal tt),
                        ("member2", mkStructType $ map dimField [1 .. n])
                       ]
        dimField n = ("member" ++ show n, 1 :# NumType Unsigned S32)

-- | Compute code for initializing one component of the weight record
initTensor :: TensorInfo -> L.ByteString
initTensor ti = (<>) "\n" $ L.unlines $ map ("  " <>)
              $ [readDims] ++ initSizes dims ++ [alloc awlPrefix] ++ readElems awlPrefix
  where prefix = "w->member" <> show (tiField ti + 1) <> ".buffer[" <> show (tiIdx ti) <> "]"
        awlPrefix = if length dims > 1 then prefix <> ".member1" else prefix
        elemTH = showElemT elemTT
        elemTC = showCElemT elemTT
        elemTT = tiType ti
        dims = D.toList $ tiDims ti
        initSizes []  = error "Zero dimensional tensor not implemented"
        initSizes [d] = [awlPrefix <> ".length = " <> show d <> ";"]
        initSizes ds  = [ prefix <> ".member2.member" <> show j <> " = " <> show n <> ";"
                        | (n,j) <- zip ds [1 :: Int ..]
                        ] ++ [awlPrefix <> ".length = " <> show (product dims) <> ";"]
        readDims = "fscanf( wfile, \" " <> elemTH <> " " <> show (length dims) <> " " <> L.unwords (map show dims) <> "\" );"
        alloc awl = awl <> ".buffer = malloc( " <> awl <> ".length * sizeof(" <> elemTC <> ") );"
        readElems awl = [ "for( int i = 0; i < " <> awl <> ".length; i++ ) {"
                        , "  fscanf( wfile, " <> scanFormat elemTT <> ", " <> awl <> ".buffer + i );"
                        , "}"
                        ]

-- | Mangle an ONNX node mane to a Feldspar identifier
mangle :: Utf8 -> L.ByteString
mangle s = "m_" <> utf8 s

showAttribute :: A.AttributeProto -> L.ByteString
showAttribute a = "(\"" <> nStr <> "\", " <> val nStr <> ")"
  where nStr = strFromJ $ A.name a
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

showAttrInts :: A.AttributeProto -> L.ByteString
showAttrInts a = "AAInts " <> listify (map show $ D.toList $ A.ints a)

showAttrInt :: A.AttributeProto -> L.ByteString
showAttrInt a = "AAInt " <> show (fromJust $ A.i a)

showAttrFloat :: A.AttributeProto -> L.ByteString
showAttrFloat a = "AAFloat " <> show (fromJust $ A.f a)

showTensorType :: (Integral a, Show b) => Maybe a -> [b] -> L.ByteString
showTensorType t dims = "Data [" <> t' <> "]" <> " -- " <> L.unwords sh
  where t' = showElemT $ int2elemT t
        sh = map show dims

int2elemT :: Integral a => Maybe a -> TD.DataType
int2elemT i = toEnum $ fromIntegral $ fromJust i :: TD.DataType

-- | The scanf() format string to read one element of the given type, including the quote marks.
--   The inttypes.h macros for format conversions of fixed width types are used.
scanFormat :: TD.DataType -> L.ByteString
scanFormat TD.FLOAT16    = error "onnxToFeld.scanFormat: FLOAT16 not implemented"
scanFormat TD.BFLOAT16   = error "onnxToFeld.scanFormat: BFLOAT16 not implemented"
scanFormat TD.FLOAT  = "\"%f\""
scanFormat TD.DOUBLE = "\"%lf\""
scanFormat TD.UINT8  = "\"%\" SCNu8"
scanFormat TD.INT8   = "\"%\" SCNd8"
scanFormat TD.UINT16 = "\"%\" SCNu16"
scanFormat TD.INT16  = "\"%\" SCNd16"
scanFormat TD.UINT32 = "\"%\" SCNu32"
scanFormat TD.INT32  = "\"%\" SCNd32"
scanFormat TD.UINT64 = "\"%\" SCNu64"
scanFormat TD.INT64  = "\"%\" SCNd64"
scanFormat TD.UNDEFINED  = error "onnxToFeld.scanFormat: UNDEFINED not implemented"
scanFormat t        = error $ "onnxTFeld.scanFormat: type not implemented:" ++ show t

-- | Map an ONNX DataType to the corresponding Type in Feldspar compiler
toType :: TD.DataType -> Type
toType TD.FLOAT16    = error "onnxToFeld.toType: FLOAT16 not implemented"
toType TD.BFLOAT16   = error "onnxToFeld.toType: BFLOAT16 not implemented"
toType TD.FLOAT      = 1 :# FloatType
toType TD.DOUBLE     = 1 :# DoubleType
toType TD.UINT8      = 1 :# NumType Unsigned S8
toType TD.INT8       = 1 :# NumType Signed   S8
toType TD.UINT16     = 1 :# NumType Unsigned S16
toType TD.INT16      = 1 :# NumType Signed   S16
toType TD.UINT32     = 1 :# NumType Unsigned S32
toType TD.INT32      = 1 :# NumType Signed   S32
toType TD.UINT64     = 1 :# NumType Unsigned S64
toType TD.INT64      = 1 :# NumType Signed   S64
toType TD.COMPLEX64  = 1 :# ComplexType (1 :# FloatType)
toType TD.COMPLEX128 = 1 :# ComplexType (1 :# DoubleType)
toType TD.STRING     = StringType
toType TD.BOOL       = 1 :# BoolType
toType t             = error $ "onnxToFeld.toType: " ++ show t ++ " not implemented"

-- | Map an ONNX DataType to a ByteString representation of the corresponding Haskell type
showElemT :: TD.DataType -> L.ByteString
showElemT TD.FLOAT16    = error "onnxToFeld.showElemT: FLOAT16 not implemented"
showElemT TD.BFLOAT16   = error "onnxToFeld.showElemT: BFLOAT16 not implemented"
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
showElemT TD.UNDEFINED  = error "onnxToFeld.showElemT: UNDEFINED not implemented"

-- | Map an ONNX DataType to a ByteString representation of the corresponding C type
showCElemT :: TD.DataType -> L.ByteString
showCElemT t = fromString $ renderType $ toType t

showType :: TV.Value -> L.ByteString
showType TV.Tensor_type{TV.tensor_type = t}
  = showTensorType (TT.elem_type t)
                   (map TSP.value $ D.toList $ TSP.dim $ fromJust $ TT.shape t)
showType TV.Sequence_type{} = "Sequence"
showType TV.Map_type{} = "Map"

showDim :: Maybe TSP.Value -> L.ByteString
showDim (Just (TSP.Dim_value i)) = show i
showDim (Just (TSP.Dim_param p)) = utf8 p
showDim Nothing = "*"

-- | Peel off a Just and convert to a ByteString
strFromJ :: Maybe Utf8 -> L.ByteString
strFromJ = utf8 . fromJust

-- | Get the name of a ValueInfoProto
vipName :: V.ValueInfoProto -> Utf8
vipName = fromJust . V.name

-- | Make a tuple expression out of a non-singleton list
tuplify :: [L.ByteString] -> L.ByteString
tuplify [s] = s
tuplify ss  = "(" <> L.intercalate ", " ss <> ")"

-- | Make a list expression
listify :: [L.ByteString] -> L.ByteString
listify ss = "[" <> L.intercalate ", " ss <> "]"

-- | An IsString-enabled version of show
show :: (Show a, IsString s) => a -> s
show = fromString . P.show
