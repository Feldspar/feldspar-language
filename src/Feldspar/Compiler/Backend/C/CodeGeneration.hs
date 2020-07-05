--
-- Copyright (c) 2009-2011, ERICSSON AB
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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Feldspar.Compiler.Backend.C.CodeGeneration where

import Prelude hiding (Semigroup(..), (<>), init)

import Feldspar.Compiler.Imperative.Representation
import Feldspar.Compiler.Imperative.Frontend (
    isNativeArray, isArray, isPointer
  )
import Feldspar.Compiler.Options

import Text.PrettyPrint

codeGenerationError :: ErrorClass -> String -> a
codeGenerationError = handleError "CodeGeneration"

data PrintEnv = PEnv
    { options :: Options
    , parNestLevel :: Int
    }

compToCWithInfos :: Options -> Module () -> String
compToCWithInfos opts procedure = render $ cgen (penv0 opts) procedure

penv0 :: Options -> PrintEnv
penv0 opts = PEnv opts 0

class CodeGen a
  where
    cgen     :: PrintEnv -> a -> Doc
    cgenList :: PrintEnv -> [a] -> Doc

instance CodeGen (Module ())
  where
    cgen env Module{..} = cgenList env entities
    cgenList env = vcat . map (cgen env)

instance CodeGen (Entity ())
  where
    cgen env StructDef{..} = text "struct"   <+> text structName     $+$ block env (cgenList env structMembers) <> semi
    cgen env TypeDef{..}   = text "typedef"  <+> cgen env actualType <+> text typeName <> semi
    cgen env Proc{..}
      | Just body <- procBody = start $$ block env (cgen env body)
      | otherwise = start <> semi
        where
         start
           | loopBody  = text ("LOOP_BODY_" ++ show (length inParams - 1))
                          <> parens (sep $ punctuate comma loopH)
           | otherwise = rtype <+> text procName <> parens (pvars env inParams)
         rtype = cgen env procType
         loopH = [text procName, text "LARGE_BODY"] ++
                  concatMap (\v ->  [cgen env (typeof v), cgen env v]) inParams
    cgen env ValueDef{..}
      | isNativeArray $ typeof valVar
      = cgen env (typeof valVar) <+> cgen env valVar <> brackets empty   <+> equals <+> cgen env valValue <> semi
      | otherwise = cgen env valVar <+> equals              <+> cgen env valValue <> semi

    cgenList env = vcat . punctuate (text "\n") . map (cgen env)

instance CodeGen (StructMember ())
  where
    cgen env StructMember{..} = cgen env structMemberType <+> text structMemberName <> sizeInBrackets structMemberType <> semi

    cgenList env = vcat . map (cgen env)

instance CodeGen (Block ())
  where
    cgen env Block{..} = vcat [ cgenList env locals
                            , if null locals then empty else text ""
                            , cgen env blockBody
                            ]
    cgenList env = vcat . map (cgen env)

instance CodeGen (Declaration ())
  where
    cgen env Declaration{..}
     | Just i <- initVal
     = var <+> nest (nestSize $ options env) equals <+> specialInit (typeof declVar) i <> semi
     | otherwise = var <+> nest (nestSize $ options env) init <> semi
      where
        var  = pvar env declVar
        init = initialize (varType declVar)
        specialInit t i
          | ConstExpr (StructConst es (StructType _ ts)) <- i
          = printStruct env es ts
          | ConstExpr (IntConst 0 _) <- i
          , isArray t || isPointer t
          = text "NULL"
          | otherwise = cgen env i

    cgenList env = vcat . map (cgen env)

instance CodeGen (Program ())
  where
    cgen _   Empty = empty
    cgen _   Comment{..}
      | isBlockComment = blockComment $ map text $ lines commentValue
      | otherwise      = text "//" <+> text commentValue
    cgen env Assign{..} = cgen env lhs <+> equals <+> nest (nestSize $ options env) (cgen env rhs) <> semi
    cgen env ProcedureCall{..} = stmt $ call (text procCallName) (map (cgen env) procCallParams)
    cgen env Sequence{..} = cgenList env sequenceProgs
    cgen env (Switch scrut [(Pat (ConstExpr (BoolConst True)), thenBlock)])
                        = text "if" <+> parens (cgen env scrut)
                       $$ block env (cgen env thenBlock)
    cgen env (Switch scrut [(Pat (ConstExpr (BoolConst True)), thenBlock),
                            (Pat (ConstExpr (BoolConst False)), elseBlock)])
                        = text "if" <+> parens (cgen env scrut)
                       $$ block env (cgen env thenBlock)
                       $$ text "else"
                       $$ block env (cgen env elseBlock)
    cgen env Switch{..} =  text "switch" <+> parens (cgen env scrutinee)
                       $$ block env (vcat [ cgen env p $$ nest (nestSize $ options env) (cgen env b $$ text "break" <> semi) | (p, b) <- alts])
    cgen env SeqLoop{..} =  cgen env sLoopCondCalc
                        $$ text "while" <+> parens (cgen env sLoopCond)
                        $$ block env (cgen env sLoopBlock $+$ cgen env sLoopCondCalc)
    cgen env ParLoop{..}
     | WorkParallel <- pParallelType
     = text "#pragma omp parallel" $$ block env forB
     | Parallel <- pParallelType
     , (name . platform . options $ env) == "c99OpenMp"
     , parNestLevel env <= 1   -- OpenMP 4 has nested data parallelism,
                               -- but it does not work well in practice.
     = text "#pragma omp parallel for" $$ forL
     | TaskParallel <- pParallelType
     , Block _ (ProcedureCall n vs) <- pLoopBlock
     = text "FOR" <> parens (sep $ punctuate comma ([text n, lstrt, loopB] ++
                                       map (cgen env) vs)) <> semi
     | otherwise = forL
      where
        forL  = text "for" <+> parens (sep $ map (nest 4) $ punctuate semi [ini, guard, next])
              $$ block env1 forB
        forB  = cgen env1 pLoopBlock
        ixd   = pvar env pLoopCounter
        ixv   = cgen env  pLoopCounter
        lstrt = cgen env pLoopStart
        ini   = ixd <+> equals    <+> lstrt
        guard = ixv <+> char '<'  <+> loopB
        loopB = cgen env pLoopEnd
        next  = ixv <+> text "+=" <+> cgen env pLoopStep
        env1 | Parallel <- pParallelType = env { parNestLevel = parNestLevel env + 1}
             | otherwise = env

    cgen env BlockProgram{..} = block env (cgen env blockProgram)

    cgenList env = vcat . map (cgen env)

instance CodeGen (Pattern ())
  where
    cgen _   PatDefault = text "default" <> colon
    cgen env (Pat c)    = text "case" <+> cgen env c <> colon

    cgenList env = vcat . map (cgen env)

instance CodeGen (ActualParameter ())
  where
    cgen env ValueParameter{..}      = cgen env valueParam
    cgen env TypeParameter{..}       = cgen env typeParam
    cgen _   FunParameter{..}        = text funParamName
    cgenList env = hsep . punctuate comma . map (cgen env)

instance CodeGen (Expression ())
  where
    cgen env VarExpr{..} = cgen env varExpr
    cgen env ArrayElem{..}
     | c@ConstExpr{} <- array
     , (NativeArray _ t) <- typeof c
     = parens (parens (cgen env t <> brackets empty) <> cgen env c)
       <> hcat (map (brackets . cgen env) arrayIndex)
     | otherwise
     = cgen env array <> hcat (map (brackets . cgen env) arrayIndex)
    cgen env StructField{..}  = parens (cgen env struct) <> char '.' <> text fieldName
    cgen env ConstExpr{..}    = cgen env constExpr
    cgen env FunctionCall{..}
        | [a,b] <- funCallParams
        , isInfixFun (funName function)
        = parens (cgen env a <+> text (funName function) <+> cgen env b)
        | otherwise                 = call (text $ funName function) $ map (cgen env) funCallParams
    cgen env Cast{..} = parens $ parens (cgen env castType) <> parens (cgen env castExpr)
    cgen env AddrOf{..} = text "&" <> cgen env addrExpr
    cgen env SizeOf{..} = call (text "sizeof") [cgen env sizeOf]
    cgen env Deref{..} = text "*" <> cgen env ptrExpr

    cgenList env = sep . punctuate comma . map (cgen env)

instance CodeGen (Variable t)
  where
    cgen _ v = text $ varName v
    cgenList env = hsep . punctuate comma . map (cgen env)

instance CodeGen (Constant ())
  where
    cgen _   (IntConst c _)     = integer c
    cgen _   (DoubleConst c)    = double c
    cgen _   (FloatConst c)     = float c
    cgen _   (BoolConst False)  = text "false"
    cgen _   (BoolConst True)   = text "true"
    cgen _   (StringConst s)    = text s
    cgen env (ArrayConst cs _)  = braces (cgenList env cs)
    cgen env (StructConst cs t) = printStruct env cs ts
      where StructType _ ts = t
    cgen env ComplexConst{..}   = parens cmplxCnst
      where cmplxCnst = cgen env realPartComplexValue <+> char '+' <+>
                        cgen env imagPartComplexValue <> char 'i'
    cgenList env = sep . punctuate comma . map (cgen env)

instance CodeGen (Maybe String, Constant ())
  where
    cgen env (n, c) = name <+> cgen env c
      where name = maybe empty (\s -> char '.' <> text s <+> equals) n
    cgenList env = hsep . punctuate comma . map (cgen env)

printStruct :: PrintEnv
            -> [(Maybe String, Constant ())] -> [(String, Type)] -> Doc
printStruct env cs ts = braces $ space <> hsep members <> space
  where members = punctuate comma $ zipWith (printStructMember env) cs ts

printStructMember :: PrintEnv
                  -> (Maybe String, Constant ()) -> (String, Type) -> Doc
printStructMember env e@(n, e') (_, t)
 | (IntConst 0 _) <- e'
 , isArray t || isPointer t
 = maybe empty (\s -> char '.' <> text s <+> equals) n <+> text "NULL"
 | otherwise = cgen env e

instance CodeGen Type
  where
    cgen _ = text . renderType

    cgenList env = sep . map (cgen env)

call :: Doc -> [Doc] -> Doc
call fn args = fn <> parens (hsep $ punctuate comma args)

-- Initializes local variables so that initArray/.. will get defined
-- inputs.
--
-- First parameter is whether the initialization context is somewhere
-- inside a compound type.
initialize :: Type -> Doc
-- Compound/Special types.
initialize (1 :# Pointer{})  = equals <+> text "NULL"
initialize ArrayType{}       = equals <+> text "NULL"
initialize NativeArray{}     = equals <+> lbrace <+> text "0" <+> rbrace
initialize StructType{}      = equals <+> lbrace <+> text "0" <+> rbrace
-- Simple types.
initialize _                 = empty

blockComment :: [Doc] -> Doc
blockComment ds = vcat (zipWith (<+>) (text "/*" : repeat (text " *")) ds)
                  $$ text " */"

sizeInBrackets :: Type -> Doc
sizeInBrackets (NativeArray l t) = brackets (maybe empty (text . show) l) <> sizeInBrackets t
sizeInBrackets _                 = empty

stmt :: Doc -> Doc
stmt = (<>semi)

block :: PrintEnv -> Doc -> Doc
block env d = lbrace $+$ nest (nestSize $ options env) d $+$ rbrace

pvar :: PrintEnv -> Variable t -> Doc
pvar env Variable{..} = typ <+> (name <> size)
  where
    typ  = cgen env varType
    size = sizeInBrackets varType
    name = text varName

pvars :: PrintEnv -> [Variable t] -> Doc
pvars env = hsep . punctuate comma . map (pvar env)

-- C operators and their precedence.
-- Precondition: s is a binop.
isInfixFun :: String -> Bool
isInfixFun s = s `elem` [
    "*", "/", "%" -- 5
  , "+", "-" -- 6
  , "<<", ">>" -- 7
  , "<", "<=", ">", ">=" -- 8
  , "==", "!=" -- 9
  , "&" -- 10
  , "^" -- 11
  , "|" -- 12
  , "&&" -- 13
  , "||" -- 14
  , "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|=" -- 16
  ]
