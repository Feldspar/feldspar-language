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

import Feldspar.Compiler.Imperative.Frontend (mkStructType, mkAwLType, uint32)
import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Options (encodeFunctionName)
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
import qualified Onnx.TypeProto as T
import qualified Onnx.TypeProto.Tensor as TT
import qualified Onnx.TypeProto.Value as TV
import qualified Onnx.ValueInfoProto as V

import Text.ProtocolBuffers (Utf8, Int64, messageGet, utf8)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as U
import qualified Data.ByteString.Lazy.Builder as B

import qualified Data.Foldable as D (toList, foldMap, length, concatMap)
import qualified Data.Sequence as D (Seq, sortBy, sort, index, partition)
import qualified Data.Map as M
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
              (useNative, shapes) = case drop 1 args of
                                        "useNative" : rest -> (True,  rest)
                                        rest               -> (False, rest)
                                    -- Subsequent arguments give input shapes
              modelBaseName = map noHyphen $ takeBaseName modelFileName
              dataFileName = modelBaseName <.> "data"
              progFileName = modelBaseName <.> "hs"
              initBaseName = modelBaseName <> "_init"
              initFileName = initBaseName <.> "c"
              cHdrFileName = modelBaseName <.> "h"
              mainBaseName = modelBaseName <> "_main"
              mainFileName = mainBaseName <.> "c"
              noHyphen c = if c == '-' then '_' else c
          modelBinary <- L.readFile modelFileName
          let model = either (\s -> error $ "onnxToFeld.main: " ++ s) fst $ messageGet modelBinary
              gr = fromMaybe (error "onnxToFeld.main: No graph in model") $ O.graph model
              (bInits,sInits) = D.partition (\ t -> product (TP.dims t) > 100) $ G.initializer gr
              proj t = (TP.data_type t, D.length $ TP.dims t)
              ordInits = D.sortBy (comparing proj) bInits
              initGroups = groupBy (\ x y -> proj x == proj y) $ D.toList ordInits
              iTensorInfo = [ [TI f i ty (TP.dims tp) (fromJust $ TP.name tp)
                               | (tp,i) <- zip tps [0 ..]]
                             | (tps,f) <- zip initGroups [0 ..],
                               let ty = int2elemT $ TP.data_type $ head tps]
              allUses = group $ D.toList $ D.sort $ foldMap N.input $ G.node gr
              multiUses = S.fromList $ map head $ filter ((>1) . length) allUses
              weightRecTC = fromString $ renderType $ mkStructType $ map toFieldType iTensorInfo
              toFieldType (ti:_) = ("member" ++ show (tiField ti + 1), mkAwLType universal $ tiToType ti)
              toFieldType []     = error "onnxToFeld.toFieldType: Impossible"
              initSet = S.fromList $ map (fromJust . TP.name) $ D.toList $ G.initializer gr
              outputs = D.toList $ G.output gr
              inputs = [p | p <- D.toList $ G.input gr, vipName p `S.notMember` initSet]
              noWeightRec = null iTensorInfo

          -- Write the weights
          dfile <- openFile dataFileName WriteMode
          hSetBinaryMode dfile True
          hSetBuffering dfile $ BlockBuffering Nothing
          B.hPutBuilder dfile $ D.foldMap buildInitTensor ordInits
          hClose dfile

          -- Write the program
          pfile <- openFile progFileName WriteMode
          L.hPutStr pfile $ mkProgramFile gr iTensorInfo sInits inputs multiUses shapes useNative
          hClose pfile

          -- Write the init module
          mfile <- openFile initFileName WriteMode
          L.hPutStr mfile $ mkInitReadFile cHdrFileName weightRecTC iTensorInfo useNative
          hClose mfile

          -- Write the main program
          mpfile <- openFile mainFileName WriteMode
          L.hPutStr mpfile
            $ mkMainFile modelBaseName cHdrFileName dataFileName weightRecTC outputs inputs noWeightRec useNative
          hClose mpfile

-- | Extract initialized tensors
buildInitTensor :: TP.TensorProto -> B.Builder
buildInitTensor t = B.lazyByteString (L.unwords $ show (length ds) : map show ds)
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
mkProgramFile
  :: G.GraphProto -> [[TensorInfo]] -> D.Seq TP.TensorProto -> [V.ValueInfoProto] -> S.Set Utf8 -> [String] -> Bool
  -> L.ByteString
mkProgramFile gr initGroups sInits inputs multiUses shapes useNative
  = L.unlines
            [ "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE TypeOperators #-}"
            , "{-# LANGUAGE FlexibleContexts #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , "{-# LANGUAGE ScopedTypeVariables #-}"
            , ""
            , "module Main where"
            , ""
            , "import Feldspar"
            , "import Feldspar.Compiler (programOpts)"
            , "import Feldspar.Core.NestedTuples"
            , "import Feldspar.Onnx.Operators"
            , "import Feldspar.Vector"
            , "import GHC.TypeLits"
            , ""
            , "type WeightRec"
            , "  = Tuple"
            , "    (" <> L.concat [mkITH ti <> " ':\n     " | ti:_ <- initGroups] <> "'[]\n    )"
            , ""
            , "main = programOpts " <> name <> " defaultOptions{printArgTypeDefs = True}" <> optModifier
            , ""
            , name <> " " <> L.unwords params <> " = " <> tuplify (map (mangle . vipName) $ D.toList $ G.output gr)
            , "  where "
              <> L.intercalate "\n        " (accesses ++ sBinds ++ inps ++ "-- Nodes" : D.concatMap (mkNode multiUses tEnv) (G.node gr))
            ]
  where name = mangle $ fromJust $ G.name gr
        params = if null initGroups then inputPs else "(weights :: WeightRec)" : inputPs
        inputPs = map mkParam inputs
        accesses = map mkAccess $ concat initGroups
        tEnv = M.fromList [(fromJust $ TP.name t, t) | t <- D.toList $ G.initializer gr]
        mkParam ti = "(" <> mangle (vipName ti) <> "' :: " <> shTy (vipType ti) <> ")"
        shTy (d,t) = "Manifest DIM" <> show d <> " (Data " <> showElemT t <> ")"
        sBinds = map mkInit $ D.toList sInits
        inps = map showInputShape inputs ++ inpBs
        inpBs = zipWith mkInputCap inputs $ map Just shapes ++ repeat Nothing
        optModifier = if useNative then "{useNativeArrays = True}" else ""

-- | Make a possibly shape contraining input binding
mkInputCap :: V.ValueInfoProto -> Maybe String -> L.ByteString
mkInputCap vi msh = mangle (vipName vi) <> " = toPull $ " <> go msh <> mangle (vipName vi) <> "'"
  where go Nothing = ""
        go (Just s) = "setSizeManifest" <> show (length sh) <> " " <> L.unwords (map show sh) <> " "
           where sh = read $ "[" <> s <> "]" :: [Int]

-- | Show shape information about an input
showInputShape :: V.ValueInfoProto -> L.ByteString
showInputShape vi = "-- " <> utf8 (vipName vi) <> " : " <> (showType $ fromJust $ T.value $ fromJust $ V.type' vi)

-- | Compute a string representation of the Haskell type of a group of initialized tensors
--   with the same dimensionality and element type.
mkITH :: TensorInfo -> L.ByteString
mkITH ti = "Pull DIM1 (DPull (" <> mkSh (D.toList $ tiDims ti) <> ") " <> eTyH <>")"
  where mkSh ds = "Z :. " <> L.intercalate " :. " ["Data Length" | _ <- ds]
        eTyH = showElemT $ tiType ti

-- | Construct a Feldspar pattern binding for an ONNX graph node
mkNode :: S.Set Utf8 -> M.Map Utf8 TP.TensorProto -> N.NodeProto -> [L.ByteString]
mkNode mUses tEnv n = [outs <> " = " <> forcing <> op <> " " <> attrs]
                   ++ [ "    " <> mangle v | v <- D.toList $ N.input n]
                   ++ [""]
  where outs = tuplify $ map mangle $ D.toList $ N.output n
        op = "onnx" <> opStr <> variant opStr
        opStr = strFromJ (N.op_type n)
        optArgOps = ["Conv"]
        attrs = "[" <> L.intercalate ", " (map showAttribute $ D.toList $ N.attribute n) <> "]"
        forcing = if any (flip S.member mUses) $ N.output n then "force $ " else ""
        variant "Reshape" = "_d" <>( show $ shapeToDim $ tEnv M.! (N.input n `D.index` 1))
        variant o | o `elem` optArgOps
                          = "_" <> show (D.length $ N.input n)
                  | otherwise = ""

-- | Compute a number of dimensions from a tensor representing a shape
shapeToDim :: TP.TensorProto -> Int64
shapeToDim p = TP.dims p `D.index` 0

-- | Read from the weight record
mkAccess :: TensorInfo -> L.ByteString
mkAccess ti = vname <> " = " <> setS <> " $ sel (Proxy @" <> show (tiField ti) <> ") weights ! (Z :. " <> show (tiIdx ti) <> ")"
  where vname = mangle $ tiName ti
        setS = "setSizePull" <> show (length dims) <> L.concat (map (\d -> " " <> show d) dims)
        dims = D.toList $ tiDims ti

-- | Initialize a small tensor
mkInit :: TP.TensorProto -> L.ByteString
mkInit t = tname <> " = arrToPull " <> ext <> " $ fromList " <> showVList (int2elemT $ TP.data_type t) t
  where tname = mangle $ fromJust $ TP.name t
        ext = "(Z" <> L.concat (map (\ d -> " :. " <> show d) $ D.toList $ TP.dims t) <> ")"

-- | Show the data in a TensorProto
showVList :: TD.DataType -> TP.TensorProto -> L.ByteString
showVList TD.FLOAT  t = show $ D.toList $ TP.float_data  t
showVList TD.DOUBLE t = show $ D.toList $ TP.double_data t
showVList TD.INT32  t = show $ D.toList $ TP.int32_data  t
showVList TD.UINT64 t = show $ D.toList $ TP.uint64_data t
showVList TD.INT64  t = show $ D.toList $ TP.int64_data  t
showVList TD.STRING t = show $ D.toList $ TP.string_data t
showVList ty        _ = error $ "OnnxToFeld.showVList: Can not handle type " <> show ty

-- | Constuct the C code to read initialized tensors
mkInitReadFile :: FilePath -> L.ByteString -> [[TensorInfo]] -> Bool -> L.ByteString
mkInitReadFile hf weightRecTC initGroups useNative
  = L.concat $ start : map initVec initGroups ++ map (initTensor useNative) (concat initGroups) ++ [end]
  where start = L.unlines
                [ "#include \"" <> fromString hf <> "\""
                , ""
                , "#include <stdlib.h>"
                , "#include <stdio.h>"
                , "#include <inttypes.h>"
                , ""
                , checkFun
                , ""
                , "struct s_0 {};"
                , ""
                , weightRecTC <> " * "
                , "read_constants(char *wfile_name) {"
                , "  FILE *wfile = fopen(wfile_name, \"r\");"
                , "  if (wfile == NULL) {"
                , "    fprintf(stderr, \"Could not open %s for reading.\\n\", wfile_name);"
                , "    exit(1);"
                , "  }"
                , ""
                , "  " <> weightRecTC <> " *"
                , "    w = malloc(sizeof(" <> weightRecTC <> "));"
                , ""
                ]
        end = L.unlines
                [ ""
                , "  fclose(wfile);"
                , "  return w;"
                , "}"
                ]
        initVec (ti:tis) = "  w->member" <> field <> ".length = " <> show (length tis + 1) <> ";\n"
                         <> "  w->member" <> field <> ".buffer = malloc(sizeof(" <> t <> ") * "
                               <> "w->member" <> field <> ".length);\n"
            where t = fromString $ renderType $ tiToType ti
                  field = show (tiField ti + 1)
        initVec [] = error "onnxToFeld.initVec: Impossible"

-- | Construct a main program
mkMainFile
  :: FilePath -> FilePath -> FilePath -> L.ByteString -> [V.ValueInfoProto] -> [V.ValueInfoProto] -> Bool -> Bool
  -> L.ByteString
mkMainFile bname hf weightFile weightRecTC outputs inputs noWeightRec useNative = start
  where start = L.unlines $
                [ "#include \"" <> fromString hf <> "\""
                , ""
                , "#include <stdlib.h>"
                , "#include <stdio.h>"
                , "#include <inttypes.h>"
                , ""
                , checkFun
                , weightRecDecls
                , "int main(int argc, char** argv) {"
                , "  if (argc < " <> show (length inputs + 1) <> ") {"
                , "    fprintf( stderr, \"Too few arguments, %d required.\\n\", " <> show (length inputs) <> ");"
                , "    exit(1);"
                , "  }"
                , ""
                ]
                ++ wread ++ concat (zipWith (mkArgRead bname useNative) inputs [1..]) ++
                [ "  " <> ot <> " " <> ov <> " = {0};"
                , ""
                , "  " <> functionName <> "(" <> warg <> inArgs <> ", &" <> ov <> ");"
                , ""
                , oCode
                , ""
                , "  return 0;"
                , "}"
                ]
        (ov, _, oCode) = mkOutput useNative outputs
        inArgs = L.intercalate ", " ["&" <> mangle (vipName v) | v <- inputs]
        functionName = fromString $ encodeFunctionName bname
        ot = argumentType bname $ length inputs + 1
        wread | noWeightRec = []
              | otherwise = ["  weight_rec_t * w = read_constants(\"" <> fromString weightFile <> "\");"
                            , ""
                            ]
        warg = if noWeightRec then "" else "w, "
        weightRecDecls
              | noWeightRec = ""
              | otherwise = L.unlines
                [ ""
                , "typedef " <> weightRecTC
                , "  weight_rec_t;"
                , ""
                , "weight_rec_t* read_constants(const char *wfile_name);"
                , ""
                ]

-- | Generate code to read the argument tensors from file
mkArgRead :: FilePath -> Bool -> V.ValueInfoProto -> Int -> [L.ByteString]
mkArgRead bname useNative vip i
                = [ "  FILE* " <> fname <> " = fopen(argv[" <> show i <> "], \"r\");"
                  , "  if (" <> fname <> " == NULL) {"
                  , "    fprintf(stderr, \"Could not open %s for reading.\\n\", argv[" <> show i <> "]);"
                  , "    exit(1);"
                  , "  }"
                  , ""
                  , "  " <> argumentType bname i <> " " <> vname <> ";"
                  , ""
                  , allocReadTensor useNative fname vname n elemT
                  , "  fclose(" <> fname <> ");"
                  , ""
                  ]
  where vname = mangle $ vipName vip
        fname = "f" <> vname
        (n, elemT) = vipType vip

-- | Argument type name
argumentType :: FilePath -> Int -> L.ByteString
argumentType bname i = fromString $ "arg_" <> show i <> "_" <> bname <> "_t"

-- | Compute the (Program) Type that corresponds to a tensor
tiToType :: TensorInfo -> Type
tiToType ti = tensorToType (D.length $ tiDims ti) (tiType ti)

-- | Compute the (Program) type corresponding to a tensor with the given dimensionality and element type
tensorToType :: Int -> TD.DataType -> Type
tensorToType n t = goDim n $ toType t
  where goDim 0 _  = error "onnxToFeld.goDim: Zero dimensional tensor not implemented"
        goDim 1 tt = mkAwLType universal tt
        goDim m tt = mkStructType
                       [("member1", mkAwLType universal tt),
                        ("member2", mkStructType $ map dimField [1 .. m])
                       ]
        dimField i = ("member" ++ show i, uint32)

-- | Compute code for initializing one component of the weight record
initTensor :: Bool -> TensorInfo -> L.ByteString
initTensor useNative ti = allocReadTensor useNative "wfile" prefix (D.length $ tiDims ti) (tiType ti) <> "\n"
  where prefix = "w->member" <> show (tiField ti + 1) <> ".buffer[" <> show (tiIdx ti) <> "]"

-- | Allocate memory for the elements of a tensor and read the contents from a file
allocReadTensor :: Bool -> L.ByteString -> L.ByteString -> Int -> TD.DataType -> L.ByteString
allocReadTensor useNative file prefix nDims elemTT = L.unlines code
  where code = map ("  " <>) $ readDims ++ alloc ++ readElems
        elemTC = showCElemT elemTT
        (awlPrefix, readDims, lenExpr) = initTensorDims useNative file prefix nDims
        alloc = if useNative
                   then []
                   else [awlPrefix <> ".buffer = malloc(" <> awlPrefix <> ".length * sizeof(" <> elemTC <> "));"]
        readElems = [ "for (int i = 0; i < " <> lenExpr <> "; i++) {"
                    , "  check(fscanf(" <> file <> ", " <> scanFormat elemTT <> ", &" <> arrVar <> "[i]), 1);"
                    , "}"
                    ]
        arrVar = if useNative then awlPrefix else awlPrefix <> ".buffer"

-- | Initialize the size of tensor from the dimensions in a file
initTensorDims :: Bool -> L.ByteString -> L.ByteString -> Int -> (L.ByteString, [L.ByteString], L.ByteString)
initTensorDims useNative file prefix 1 = (prefix, scanCode, lenExpr)
  where (scanCode, lenExpr) | useNative = ([mkScanf file ["dummyInt"]], staticSize prefix)
                            | otherwise = ([mkScanf file [prefix <> ".length"]], prefix <> ".length")
initTensorDims useNative file prefix n = (prefix <> ".member1", mkScanf file vars : lenUpdate, lenExpr)
  where (lenUpdate, lenExpr) | useNative = ([], staticSize $ prefix <> ".member1")
                             | otherwise = ([prefix <> ".member1.length = " <> L.intercalate " * " vars <> ";"],
                                            prefix <> ".member1.length")
        vars = reverse [prefix <> ".member2.member" <> show i | i <- [1 :: Int .. n]]

-- | Static expression for length of native array
staticSize :: L.ByteString -> L.ByteString
staticSize prefix = "(sizeof(" <> prefix <> ") / sizeof(" <> prefix <> "[0]))"


-- | Generate a call to fscanf reading the tensor dimensions
mkScanf :: L.ByteString -> [L.ByteString] -> L.ByteString
mkScanf file vars = "check(fscanf(" <> file <> ", " <> format <> ptrs <> "), " <> show n <> ");"
  where format = "\" " <> show n <> L.concat (replicate n " %\" SCNu32 \"") <> "\""
        ptrs = L.concat [", &" <> v | v <- vars]
        n = length vars

-- | Generate a variable name, a type and code for printing output
mkOutput :: Bool -> [V.ValueInfoProto] -> (L.ByteString, L.ByteString, L.ByteString)
mkOutput useNative outputs = go outputs
  where go [vip] = (v, fromString $ renderType t, printTensor useNative "stdout" v dt)
          where dt = vipType vip
                t = uncurry tensorToType dt
                v = mangle $ vipName vip
        go _ = error "onnxToFeld.mkOutput: Only a single output currently supported"

-- | Generate code to print a text representation of a tensor to a file
printTensor :: Bool -> L.ByteString -> L.ByteString -> (Int, TD.DataType) -> L.ByteString
printTensor useNative file prefix (1,t)
                              = printTensorDims file 1 [prefix <> ".length"]
                             <> printTensorElems useNative file prefix t
printTensor useNative file prefix (n,t)
                              = printTensorDims file n vars
                             <> printTensorElems useNative file (prefix <> ".member1") t
  where vars = [prefix <> ".member2.member" <> show i | i <- reverse [1..n]]

-- | Generate code to print the tensor dimensions to a file
printTensorDims :: L.ByteString -> Int -> [L.ByteString] -> L.ByteString
printTensorDims file n vars = "  fprintf(" <> file <> ", " <> format <> ", " <> args <> ");\n"
  where format = "\"" <> show n <> L.concat (replicate n " %\" PRIu32 \"") <> "\\n\""
        args = L.intercalate ", " vars

-- | Generate code to print the elements of a tensor to a file
printTensorElems :: Bool -> L.ByteString -> L.ByteString -> TD.DataType -> L.ByteString
printTensorElems useNative file prefix t = L.unlines ls
  where ls = [ "  for (int i = 0; i < " <> lenExpr <> "; i++) {"
             , "    fprintf(" <> file <> ", " <> printFormat t <> " \"\\n\", " <> elemExpr <> "[i]);"
             , "  }"
             ]
        lenExpr = if useNative then staticSize prefix else prefix <> ".length"
        elemExpr = if useNative then prefix else prefix <> ".buffer"

checkFun :: L.ByteString
checkFun = L.unlines
         [ "static inline void check(int n, int ref) {"
         , "  static int items = 0;"
         , "  if (n != ref) {"
         , "    fprintf(stderr, \"I/O error: check failed, n = %d ref = %d after reading %d items\\n\","
         , "                    n, ref, items );"
         , "    exit(1);"
         , "  }"
         , "  items++;"
         , "}"
         ]

-- | Mangle an ONNX node mane to a Feldspar identifier
mangle :: Utf8 -> L.ByteString
mangle s = "m_" <> L.concatMap subst (utf8 s)
  where subst '/'  = "\'s"
        subst ':'  = "\'c"
        subst '\'' = "\'\'"
        subst c    = L.singleton c

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
        -- Flatten
        val "axis"         = showAttrInt a
        -- Not found
        val _ = "_|_"

showAttrInts :: A.AttributeProto -> L.ByteString
showAttrInts a = "AAInts " <> listify (map show $ D.toList $ A.ints a)

showAttrInt :: A.AttributeProto -> L.ByteString
showAttrInt a = "AAInt " <> show (fromJust $ A.i a)

showAttrFloat :: A.AttributeProto -> L.ByteString
showAttrFloat a = "AAFloat " <> show (fromJust $ A.f a)

showTensorType :: Integral a => Maybe a -> [Maybe TSP.Value] -> L.ByteString
showTensorType t dims = "Tensor " <> t' <> " " <> L.unwords sh
  where t' = showElemT $ int2elemT t
        sh = map showDim dims

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

-- | The printf() format string to write one element of the given type, including the quote marks.
--   The inttypes.h macros for format conversions of fixed width types are used.
printFormat :: TD.DataType -> L.ByteString
printFormat TD.FLOAT16    = error "onnxToFeld.printFormat: FLOAT16 not implemented"
printFormat TD.BFLOAT16   = error "onnxToFeld.printFormat: BFLOAT16 not implemented"
printFormat TD.FLOAT  = "\"%f\""
printFormat TD.DOUBLE = "\"%lf\""
printFormat TD.UINT8  = "\"%\" PRIu8"
printFormat TD.INT8   = "\"%\" PRId8"
printFormat TD.UINT16 = "\"%\" PRIu16"
printFormat TD.INT16  = "\"%\" PRId16"
printFormat TD.UINT32 = "\"%\" PRIu32"
printFormat TD.INT32  = "\"%\" PRId32"
printFormat TD.UINT64 = "\"%\" PRIu64"
printFormat TD.INT64  = "\"%\" PRId64"
printFormat TD.UNDEFINED  = error "onnxToFeld.printFormat: UNDEFINED not implemented"
printFormat t        = error $ "onnxTFeld.printFormat: type not implemented:" ++ show t

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

-- | Find the dimension (shape length) and element type of a ValueInfoProto
vipType :: V.ValueInfoProto -> (Int, TD.DataType)
vipType = go . fromJust . T.value . fromJust . V.type'
  where go TV.Tensor_type{TV.tensor_type = t}
           = (D.length $ TSP.dim $ fromJust $ TT.shape t, int2elemT $ TT.elem_type t)
        go _ = error "onnxToFeld.vipType: not a tensor"

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
