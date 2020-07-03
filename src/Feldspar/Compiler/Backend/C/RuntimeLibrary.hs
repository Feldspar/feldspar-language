{-# OPTIONS_GHC -Wall #-}
-- FIXME: Replace this module by splicing in ExternalProgram-input
--        into the compilation chain.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Feldspar.Compiler.Backend.C.RuntimeLibrary where

import Data.Maybe (fromJust)
import Text.PrettyPrint (render)

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Backend.C.CodeGeneration (cgen, penv0)
import Feldspar.Compiler.Options hiding (name)

machineLibrary :: Options -> [Entity ()]
machineLibrary opts =
  map (\t -> abs_fun opts (1 :# NumType Signed t)) sizes ++
  map (\t -> pow_fun opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> pow_fun opts (1 :# NumType Signed t)) sizes ++
  map (\t -> signum_fun_u opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> signum_fun_s opts (1 :# NumType Signed t)) sizes ++
  map (\t -> signum_fun_f opts (1 :# t)) floats ++
  map (\t -> logbase_fun_f opts (1 :# t)) floats ++
  map (\t -> setBit_fun opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> setBit_fun opts (1 :# NumType Signed t)) sizes ++
  map (\t -> clearBit_fun opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> clearBit_fun opts (1 :# NumType Signed t)) sizes ++
  map (\t -> complementBit_fun opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> complementBit_fun opts (1 :# NumType Signed t)) sizes ++
  map (\t -> testBit_fun opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> testBit_fun opts (1 :# NumType Signed t)) sizes ++
  map (\t -> rotateL_fun_u opts (1 :# NumType Unsigned t)) sizes ++
--  map (\t -> rotateL_fun_s opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> rotateR_fun_u opts (1 :# NumType Unsigned t)) sizes ++
--  map (\t -> rotateR_fun_s opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> reverseBits_fun_u opts (1 :# NumType Unsigned t)) sizes ++
--  map (\t -> rotateR_fun_s opts (1 :# NumType Unsigned t)) sizes ++
  map (\t -> bitScan_fun_u opts (1 :# NumType Unsigned t)) sizes
--  map (\t -> bitScan_fun_s opts (1 :# NumType Unsigned t)) sizes ++

sizes :: [Size]
sizes = [S8, S16, S32, S64]

floats :: [ScalarType]
floats = [FloatType, DoubleType]

{-
int8_t abs_fun_int8_t( int8_t a ) {
    // From Bit Twiddling Hacks:
    //    "Compute the integer absolute value (abs) without branching"
    int8_t mask = a >> 7;
    return (a + mask) ^ mask;
}
-}

abs_fun :: Options -> Type -> Entity ()
abs_fun opts typ = Proc name False [inVar] (typeof outVar) (Just body)
 where name   = "abs_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = Block lvars prg
       mVar   = Variable typ "mask"
       mVar'  = varToExpr mVar
       prg    = call "return" [ValueParameter $ binop typ "^" (binop typ "+" inVar' mVar') mVar']
       lvars  = [Declaration mVar (Just ival)]
       ival   = binop typ ">>" inVar' arg2
       arg2   = ConstExpr (IntConst (sizeToNum typ - 1) typ')
       typ'   = case typ of
                  _ :# t -> t

{-
uint8_t pow_fun_uint8_t( uint8_t a, uint8_t b ) {
    uint8_t r = 1;
    int i;
    for(i = 0; i < b; ++i)
        r *= a;
    return r;
}
int8_t pow_fun_int8_t( int8_t a, int8_t b ) {
    int8_t r = 1;
    int i;
    if (b < 0) {
        fprintf(stderr, "Negative exponent in function pow_fun_(): %d `pow` %d", a, b);
        exit(1);
    }
    for(i = 0; i < b; ++i)
        r *= a;
    return r;
}
-}

pow_fun :: Options -> Type -> Entity ()
pow_fun opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "pow_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1 = Variable typ "a"
       inVar1'= varToExpr inVar1
       inVar2 = Variable typ "b"
       inVar2'= varToExpr inVar2
       inVar3 = Variable typ "i"
       outVar = Variable typ "r"
       outVar'= varToExpr outVar
       lvars  = [Declaration outVar $ Just (litI typ 1)]
       body   = Block lvars prg
       prg    = Sequence [ guard
                         , for Sequential inVar3 (litI typ 0) inVar2' (litI typ 1) (Block [] body')
                         , call "return" [ValueParameter $ outVar']]
       body'  = Assign outVar' (binop typ "*" outVar' inVar1')
       guard
         | Just True <- intSigned typ
         = Sequence [ call "assert" [ ValueParameter $
                         binop (1 :# BoolType) "<" inVar2' (litI typ 0)]
                    , Comment False "Negative exponent in function pow_fun"]
         | otherwise = Empty

{-
int8_t signum_fun_int8_t( int8_t a ) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a != 0) | (a >> 7);
}
-}

signum_fun_s :: Options -> Type -> Entity ()
signum_fun_s opts typ = Proc name False [inVar] (typeof outVar) (Just body)
 where name   = "signum_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ "|" (binop typ "!=" inVar' (litI typ 0)) ival]
       ival   = binop typ ">>" inVar' arg2
       arg2   = ConstExpr (IntConst (sizeToNum typ - 1) typ')
       typ'   = case typ of
                  _ :# t -> t

{-
uint8_t signum_fun_uint8_t( uint8_t a ) {
    return (a > 0);
}
-}

signum_fun_u :: Options -> Type -> Entity ()
signum_fun_u opts typ = Proc name False [inVar] (typeof outVar) (Just body)
 where name   = "signum_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ ">" inVar' (litI typ 0)]

{-
float signum_fun_float( float a ) {
    // From Bit Twiddling Hacks: "Compute the sign of an integer"
    return (a > 0) - (a < 0);
}
-}

signum_fun_f :: Options -> Type -> Entity ()
signum_fun_f opts typ = Proc name False [inVar] (typeof outVar) (Just body)
 where name   = "signum_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "a"
       inVar' = varToExpr inVar
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ "-" gzero szero]
       zero   = litI typ 0
       gzero  = binop typ ">" inVar' zero
       szero  = binop typ "<" inVar' zero


{-
float logBase_fun_float( float a, float b ) {
    return logf(b) / logf(a);
}
-}
logbase_fun_f :: Options -> Type -> Entity ()
logbase_fun_f opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "logbase_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "a"
       inVar1' = varToExpr inVar1
       inVar2  = Variable typ "b"
       inVar2' = varToExpr inVar2
       outVar = Variable typ "out"
       body   = toBlock prg
       prg    = call "return" [ValueParameter $
                   binop typ "/" numer denom]
       numer  = fun typ (logNam typ) [inVar2']
       denom  = fun typ (logNam typ) [inVar1']
       logNam (_ :# FloatType)  = "logf"
       logNam (_ :# DoubleType) = "log"

{-
int8_t setBit_fun_int8_t( int8_t x, uint32_t i ) {
    return x | 1 << i;
}
-}
setBit_fun :: Options -> Type -> Entity ()
setBit_fun opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "setBit_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "x"
       inVar1' = varToExpr inVar1
       inVar2  = Variable (1 :# (NumType Unsigned S32)) "i"
       inVar2' = varToExpr inVar2
       outVar  = Variable typ "out"
       body    = toBlock prg
       prg     = call "return" [ValueParameter $
                   binop typ "|" inVar1' ival]
       ival    = binop typ "<<" (litI typ 1) inVar2'

{-
int8_t clearBit_fun_int8_t( int8_t x, uint32_t i ) {
    return x & ~(1 << i);
}
-}
clearBit_fun :: Options -> Type -> Entity ()
clearBit_fun opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "clearBit_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "x"
       inVar1' = varToExpr inVar1
       inVar2  = Variable (1 :# (NumType Unsigned S32)) "i"
       inVar2' = varToExpr inVar2
       outVar  = Variable typ "out"
       body    = toBlock prg
       prg     = call "return" [ValueParameter $
                   binop typ "&" inVar1' (fun typ "~" [ival])]
       ival    = binop typ "<<" (litI typ 1) inVar2'

{-
int8_t complementBit_fun_int8_t( int8_t x, uint32_t i ) {
    return x ^ 1 << i;
}
-}
complementBit_fun :: Options -> Type -> Entity ()
complementBit_fun opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "complementBit_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "x"
       inVar1' = varToExpr inVar1
       inVar2  = Variable (1 :# (NumType Unsigned S32)) "i"
       inVar2' = varToExpr inVar2
       outVar  = Variable typ "out"
       body    = toBlock prg
       prg     = call "return" [ValueParameter $
                   binop typ "^" inVar1' ival]
       ival    = binop typ "<<" (litI typ 1) inVar2'

{-
bool testBit_fun_int8_t( int8_t x, uint32_t i )
{
    return (x & 1 << i) != 0;
}
-}
testBit_fun :: Options -> Type -> Entity ()
testBit_fun opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "testBit_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "x"
       inVar1' = varToExpr inVar1
       inVar2  = Variable (1 :# (NumType Unsigned S32)) "i"
       inVar2' = varToExpr inVar2
       outVar  = Variable (1 :# BoolType) "out"
       body    = toBlock prg
       prg     = call "return" [ValueParameter $
                   binop btyp "!=" (binop typ "&" inVar1' ival) (litI typ 0)]
       ival    = binop typ "<<" (litI typ 1) inVar2'
       btyp    = 1 :# BoolType

-- TODO: rotateL signed

{-
uint8_t rotateL_fun_uint8_t( uint8_t x, int32_t i ) {
    if ((i %= 8) == 0) return x;
    return (x << i) | (x >> (8 - i));
}
-}
rotateL_fun_u :: Options -> Type -> Entity ()
rotateL_fun_u opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "rotateL_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "x"
       inVar1' = varToExpr inVar1
       inVar2  = Variable (1 :# (NumType Unsigned S32)) "i"
       inVar2' = varToExpr inVar2
       outVar  = Variable typ "out"
       body    = toBlock prg
       ret1    = call "return" [ValueParameter inVar1']
       cnd     = binop (1 :# BoolType) "=="
                     (binop typ "%=" inVar2' (litI typ sz)) (litI typ 0)
       prg     = Sequence [mkIf cnd (toBlock ret1) Nothing,
                  call "return" [ValueParameter $
                    binop typ "|" (binop typ "<<" inVar1' inVar2') rsh]]
       rsh     = binop typ ">>" inVar1' (binop typ "-" arg2 inVar2')
       arg2    = ConstExpr (IntConst sz typ')
       sz      = sizeToNum typ
       typ'    = case typ of
                  _ :# t -> t

-- TODO: rotateR signed

{-
uint8_t rotateR_fun_uint8_t( uint8_t x, int32_t i ) {
    if ((i %= 8) == 0) return x;
    return (x << (8 - i)) | (x >> i);
}
-}
rotateR_fun_u :: Options -> Type -> Entity ()
rotateR_fun_u opts typ = Proc name False [inVar1, inVar2] (typeof outVar) (Just body)
 where name   = "rotateR_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar1  = Variable typ "x"
       inVar1' = varToExpr inVar1
       inVar2  = Variable (1 :# (NumType Unsigned S32)) "i"
       inVar2' = varToExpr inVar2
       outVar  = Variable typ "out"
       body    = toBlock prg
       ret1    = call "return" [ValueParameter inVar1']
       cnd     = binop (1 :# BoolType) "=="
                     (binop typ "%=" inVar2' (litI typ sz)) (litI typ 0)
       prg     = Sequence [mkIf cnd (toBlock ret1) Nothing,
                  call "return" [ValueParameter $
                    binop typ "|" lsh (binop typ ">>" inVar1' inVar2') ]]
       lsh     = binop typ "<<" inVar1' (binop typ "-" arg2 inVar2')
       arg2    = ConstExpr (IntConst sz typ')
       sz      = sizeToNum typ
       typ'    = case typ of
                  _ :# t -> t

-- TODO: reverseBits signed

{-
uint8_t reverseBits_fun_uint8_t( uint8_t x ) {
    uint8_t r = x;
    int i = 7;
    while (x >>= 1)
    {
        r = (r << 1) | (x & 1);
        --i;
    }
    return r << i;
}
-}

reverseBits_fun_u :: Options -> Type -> Entity ()
reverseBits_fun_u opts typ = Proc name False [inVar] (typeof outVar) (Just body)
 where name   = "reverseBits_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "x"
       inVar' = varToExpr inVar
       outVar = Variable typ "r"
       outVar'= varToExpr outVar
       ivar   = Variable typ "i"
       ivar'  = varToExpr ivar
       lvars  = [ Declaration outVar $ Just inVar'
                , Declaration ivar $ Just $ litI typ (sz - 1)]
       body   = Block lvars prg
       empty  = toBlock Empty
       prg    = Sequence [ while empty (binop typ ">>=" inVar' (litI typ 1)) body'
                         , call "return" [ValueParameter $
                                             binop typ "<<" outVar' ivar']]
       body'   = toBlock $ Sequence [ Assign outVar' shift
                                    , Assign ivar' (binop typ "-" ivar' (litI typ 1))]
       shift   = binop typ "|" (binop typ "<<" outVar' (litI typ 1))
                               (binop typ "&" inVar' (litI typ 1))
       sz      = sizeToNum typ

-- TODO: bitScan for signed.

{-
uint32_t bitScan_fun_uint8_t( uint8_t x ) {
    uint32_t r = 8;
    while (x) {
        --r;
        x >>= 1;
    }
    return r;
}
-}
bitScan_fun_u :: Options -> Type -> Entity ()
bitScan_fun_u opts typ = Proc name False [inVar] (typeof outVar) (Just body)
 where name   = "bitScan_fun_" ++ (render $ cgen (penv0 opts) typ)
       inVar  = Variable typ "x"
       inVar' = varToExpr inVar
       ot     = 1 :# (NumType Unsigned S32)
       outVar = Variable ot "r"
       outVar'= varToExpr outVar
       lvars  = [Declaration outVar $ Just $ litI ot sz]
       body   = Block lvars prg
       empty  = toBlock Empty
       prg    = Sequence [ while empty inVar' body'
                         , call "return" [ValueParameter outVar']]
       body'   = toBlock $ Sequence [ Assign outVar' (binop ot "-" outVar' (litI typ 1))
                                    , Assign inVar' shift ]
       shift   = binop typ ">>" inVar' (litI typ 1)
       sz      = sizeToNum typ

-- TODO:  bitCount and forward in feldspar_c99.c

sizeToNum :: Type -> Integer
sizeToNum = fromJust . intWidth

stderr :: Expression ()
stderr = varToExpr $ Variable VoidType "stderr"
