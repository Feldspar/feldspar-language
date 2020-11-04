{-# OPTIONS_GHC -Wall #-}
-- Partial functions due to the mismatch between Program and C.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Feldspar.Compiler.Imperative.ExternalProgram (parseFile, massageInput) where

import Feldspar.Lattice (universal)
import qualified Feldspar.Compiler.Imperative.Representation as R
import Feldspar.Compiler.Imperative.Representation hiding (
  Block, Switch, Assign, Cast, IntConst, FloatConst, DoubleConst, StringConst,
  Type, Deref, AddrOf, Unsigned, Signed, inParams)
import Feldspar.Compiler.Imperative.Frontend (
    litB, litI32, toBlock, fun, call, decodeType
  )

import qualified Data.ByteString.Char8 as B
import qualified Language.C.Parser as P
import Language.C.Syntax
import Data.List (intersperse, mapAccumL, isPrefixOf)
import Data.Loc
import Data.Maybe

-- Necessary for now.
import Debug.Trace

-- The parser is quite general and handles ObjC and has support for
-- AntiQuotations. We are interested in parsing "External Program"
-- which grammatically is a subset of C, so most functions in this
-- file match on some constructors and end with a general error case
-- that tells which function it is and shows the input it got.
--
-- The intention is that these errors should only capture the
-- constructors that are outside the C language, and valid "External
-- Program" programs will never see them. The second intention is that
-- these error messages will be unique and easy to pinpoint in the case
-- that bugs do occur.

-- | Parse a file and return a Module representing the file
parseFile :: FilePath -> B.ByteString -> [Entity] -> Maybe Module
parseFile filename s hDefs =
  case P.parse [C99,OpenCL] builtinTypes P.parseUnit s' (Just $ startPos filename) of
      Left _ -> Nothing
      Right defs -> Just (Module $ fhDefs ++ toProgram hDefs defs)
   where s' = massageInput s
         fhDefs = filter isStructDef hDefs
         isStructDef StructDef{} = True
         isStructDef _           = False

-- | A list of built in types. We need the regular C99 types in this list, or
-- the parser will die with a mysterious parse error.
builtinTypes :: [String]
builtinTypes = [ "uint64_t", "uint32_t", "uint16_t", "uint8_t"
               , "int64_t", "int32_t", "int16_t", "int8_t"
               , "bool"]

toProgram :: [Entity] -> [Definition] -> [Entity]
toProgram [] defs = snd $ defsToProgram (emptyEnv []) defs
toProgram hDefs defs = rest' ++ reverse funcs'
  where funcs' = snd $ defsToProgram env' $ reverse funcs
        (env', rest') = defsToProgram (emptyEnv (patchHdefs hDefs)) rest
        (funcs, rest) = span isFunc defs
        isFunc FuncDef{} = True
        isFunc _         = False

defsToProgram :: TPEnv -> [Definition] -> (TPEnv, [Entity])
defsToProgram = mapAccumL defToProgram

defToProgram :: TPEnv -> Definition -> (TPEnv, Entity)
defToProgram env (FuncDef func _) = funcToProgram env func
defToProgram env (DecDef (InitGroup ds _ is@[Init n Array{} Nothing (Just (CompoundInitializer ins _)) _ _] _) _)
  = valueToProgram env ds is n ins'
   where ins' = map snd ins
defToProgram env (DecDef ig _) = initGroupToDeclaration env ig
defToProgram _ (EscDef s _) = error ("defToProgram: " ++ show s)
defToProgram _ e = error ("defToProgram: Unhandled construct: " ++ show e)

funcToProgram :: TPEnv -> Func -> (TPEnv, Entity)
funcToProgram env (Func ds name _ (Params parms _ _) bis _)
  = (env'', Proc (unId name) False vs dsl (Just bs))
   where (env', vs) = mapAccumL paramToVariable env parms
         (env'', bs) = blockToBlock env' bis
         dsl = declSpecToType env ds
funcToProgram _ e = error ("funcToProgram: Unhandled construct: " ++ show e)

valueToProgram :: TPEnv -> DeclSpec -> [Init] -> Id -> [Initializer]
               -> (TPEnv, Entity)
valueToProgram env ds is n ins = (env', ValueDef (nameToVariable env' n) cs')
  where env' = fst $ initToNames env ds is
        cs = map (\(ExpInitializer (Const c _) _) -> constToConstant c) ins
        cs' = ArrayConst (map (castConstant t) cs) t
        t = declSpecToType env ds

paramToVariable :: TPEnv -> Param -> (TPEnv, R.Variable)
paramToVariable env (Param (Just id') t p _)
 | Just v' <- lookup (unId id') (vars env) = (env, v') -- We have recovered types.
 | otherwise = (updateEnv env [v], v)
  where v = Variable (declToType (declSpecToType env t) p) (unId id')
paramToVariable _ e = error $ "paramToVariable: Unhandled construct: " ++ show e

blockToBlock :: TPEnv -> [BlockItem] -> (TPEnv, R.Block)
blockToBlock env bis = (env'', R.Block (concat ds) (Sequence bs))
  where (env', ds) = mapAccumL blockDeclToDecl env decls
        (env'', bs) = blockItemsToProgram env' rest
        (decls, rest) = span isBlockDecl bis
        isBlockDecl BlockDecl{} = True
        isBlockDecl _           = False

blockDeclToDecl :: TPEnv -> BlockItem -> (TPEnv, [Declaration])
blockDeclToDecl env (BlockDecl ig) = initGroupToProgram env ig

blockItemsToProgram :: TPEnv -> [BlockItem] -> (TPEnv, [Program])
-- TODO: Stop freeloading on ParLoop since we have to fake v/t at this stage.
blockItemsToProgram env (BlockStm (Pragma "omp parallel" _):bis)
  = (env', [ParLoop WorkParallel v t t t (toBlock $ Sequence ps)])
    where (env', ps) = blockItemsToProgram env bis
          t = litB True -- Fake information with the correct type.
          v = snd $ head builtins -- Fake information with the correct type.
blockItemsToProgram env bs = mapAccumL blockItemToProgram env bs

blockItemToProgram :: TPEnv -> BlockItem -> (TPEnv, Program)
blockItemToProgram _ b@BlockDecl{}
  = error ("Declaration in the middle of a block: " ++ show b)
-- Ivar reconstruction stuff.
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_get" _) _) _ _)) _))
  = (env, ProcedureCall "ivar_get" (tp:map ValueParameter es))
    where (FunctionCall (Function "ivar_get" _) es) = expToExpression env e
          tp = TypeParameter $ typeof (R.Deref (head es))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_get_nontask" _) _) _ _)) _))
  = (env, ProcedureCall "ivar_get_nontask" (tp:map ValueParameter es))
    where (FunctionCall (Function "ivar_get_nontask" _) es) = expToExpression env e
          tp = TypeParameter $ typeof (R.Deref (head es))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "ivar_put" _) _) _ _)) _))
  = (env, ProcedureCall s (tp:map ValueParameter es))
   where (FunctionCall (Function s _) es) = expToExpression env e
         tp = TypeParameter (typeof (R.Deref (last es)))
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run2" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run3" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run4" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "run5" _) _) _ _)) _))
  = (env, ProcedureCall s (FunParameter e1:tp))
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1)]) = expToExpression env e
         tp = map TypeParameter $ lookup3 e1 (headerDefs env)
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn2" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1),e2,e3]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3]
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn3" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1),e2,e3,e4]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3
              , TypeParameter (typeof e4), ValueParameter e4]
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn4" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1), e2, e3, e4, e5]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3
              , TypeParameter (typeof e4), ValueParameter e4
              , TypeParameter (typeof e5), ValueParameter e5]
blockItemToProgram env (BlockStm (Exp (Just e@(FnCall (Var (Id "spawn5" _) _) _ _)) _))
  = (env, ProcedureCall s es)
   where (FunctionCall (Function s _) [VarExpr (Variable _ e1), e2, e3, e4, e5, e6]) = expToExpression env e
         es = [ FunParameter e1, TypeParameter (typeof e2), ValueParameter e2
              , TypeParameter (typeof e3), ValueParameter e3
              , TypeParameter (typeof e4), ValueParameter e4
              , TypeParameter (typeof e5), ValueParameter e5
              , TypeParameter (typeof e6), ValueParameter e6]
-- Probably copyArray.
blockItemToProgram env (BlockStm (Exp (Just e@FnCall{}) _))
  = (env, ProcedureCall s (map ValueParameter es))
   where (FunctionCall (Function s _) es) = expToExpression env e
blockItemToProgram env (BlockStm (Exp (Just e) _)) = impureExpToProgram env e
blockItemToProgram env (BlockStm e) = (env, stmToProgram env e)
blockItemToProgram _ e = error ("blockItemsToProgram: Unhandled construct: " ++ show e)

initGroupToDeclaration :: TPEnv -> InitGroup -> (TPEnv, Entity)
-- Struct definitions and similar.
initGroupToDeclaration env (InitGroup ds _ [] _) = (env', s)
  where env' = updateEnv2 env [] t
        t = declSpecToType env ds
        [s] = mkDef t
initGroupToDeclaration env (TypedefGroup ds _ (h:_) _) = (env', s)
  where env'                  = updateEnv2 env [] t
        (StructType _ fields) = declSpecToType env ds
        t                     = StructType (typeDefToName h) fields
        [s]                   = mkDef t
-- Function declarations
initGroupToDeclaration env (InitGroup ds _ [is@(Init _ (Proto _ (Params ps _ _) _) _ _ _ _)] _)
  = initToFunDecl env ds is ps
initGroupToDeclaration _ e = error $ "initGroupToDeclaration: " ++ show e

initGroupToProgram :: TPEnv -> InitGroup -> (TPEnv, [Declaration])
-- Variable declarations
initGroupToProgram env (InitGroup ds _ is _) = initToNames env ds is
initGroupToProgram _ (TypedefGroup _ds _attr _ts _)
  = error "initGroupToProgram: hit TypedefGroup."
initGroupToProgram _ e
  = error $ "initGroupToProgram: Unhandled construct: " ++ show e

-- Contrived type to swallow break statements silently.
switchAltToProgram :: TPEnv -> BlockItem -> [(Pattern, R.Block)]
switchAltToProgram _ (BlockStm Break{}) = []
switchAltToProgram env (BlockStm (Case e s _))
  = [(Pat (expToExpression env e), toBlock (stmToProgram env s))]
switchAltToProgram env (BlockStm (Default stm _))
  = [(PatDefault, toBlock (stmToProgram env stm))]
switchAltToProgram _ e = error $ "switchAltToProgram: " ++ show e

stmToProgram :: TPEnv -> Stm -> Program
stmToProgram _ l@Label{} = error $ "stmToProgram: Unexpected label: " ++ show l
stmToProgram _ (Exp Nothing _) = error "Exp: Nothing?"
stmToProgram env (Exp (Just e) _) = snd $ impureExpToProgram env e
stmToProgram env (Block bis _) = R.BlockProgram $ snd $ blockToBlock env bis
stmToProgram env (If e b1 (Just b2) _)
  = R.Switch cond [ (Pat $ litB True, toBlock $ stmToProgram env b1)
                  , (Pat $ litB False, toBlock $ stmToProgram env b2)]
    where cond = expToExpression env e
stmToProgram env (Switch e (Block alts' _) _)
  = R.Switch (expToExpression env e) $ concatMap (switchAltToProgram env) alts'
stmToProgram env (While e s _) = SeqLoop cond (toBlock Empty) (toBlock p)
  where cond = expToExpression env e
        p = stmToProgram env s
stmToProgram _ (DoWhile _s _e _) = error "stmToProgram: No support for Do."
stmToProgram env (For eit
                      (Just (BinOp Lt Var{} v2 _))
                      (Just ass) s _)
  = ParLoop Sequential v' e0' (expToExpression env' v2) rhs' body
    where body = toBlock $ stmToProgram env' s
          (v', env', e0') = case eit of
             Right (Just (Assign name JustAssign e0 _)) -> (v, env, expToExpression env e0)
               where v = varToVariable env name
             Left es@InitGroup{} -> (v, env0, e0)
               where (env0, es') = initGroupToProgram env es
                     [Declaration v (Just e0)] = es'
             _ -> error $ "stmToProgram: Unknown first for block: " ++ show eit
          rhs' = case ass of
                   Assign _ AddAssign rhs'' _ -> expToExpression env' rhs''
                   PostInc v _              -> plusOne $ expToExpression env' v
                   _ -> error $ "stmToProgram: Unknown second for block: " ++ show ass
stmToProgram _ Goto{} = error "stmToProgram: No support for goto."
stmToProgram _ Continue{} = error "stmToProgram: No support for continue."
stmToProgram _ Break{} = error "stmToProgram: Unexpected break."
stmToProgram env (Return (Just e) _)
  = call "return" [ValueParameter $ expToExpression env e]
stmToProgram _ (Pragma _ _) = error "Pragma not supported yet."
stmToProgram _ a@Asm{} = error $ "stmToProgram: unexpected asm: " ++ show a
stmToProgram _ e = error $ "stmToProgram: Unhandled construct: " ++ show e

impureExpToProgram :: TPEnv -> Exp -> (TPEnv, Program)
-- Hook for padding incomplete struct array * type info.
impureExpToProgram env (Assign e JustAssign
                               f@(FnCall (Var (Id "initArray" _) _)
                                         [_, e2', _] _) _)
  = (env', R.Assign (expToExpression env' e) (expToExpression env' f))
   where env' = fixupEnv env e $ typToType env (getTyp e2')
         getTyp (SizeofType e' _) = e'
         getTyp (BinOp Sub _ e' _) = getTyp e'
         getTyp e' = error $ "Unexpected parameter to initArray:" ++ show e'
impureExpToProgram env (Assign e@Var{} JustAssign
                               f@(FnCall (Var (Id "at" _) _) [e1, _] _) _)
  = (env', R.Assign (expToExpression env e) (expToExpression env' f))
   where env' = fixupEnv env e1 $ varType (varToVariable env e)
impureExpToProgram env (Assign e@(FnCall (Var (Id "at" _) _) [e1, _] _) JustAssign
                               f@(FnCall (Var (Id "at" _) _) [e1', _] _) _)
  = (env'', R.Assign (expToExpression env'' e) (expToExpression env'' f))
   -- Hope LHS or RHS has proper types. Propagate to the other side.
   where env'' = fixupEnv env' e1 $ typeof (expToExpression env' f)
         env'  = fixupEnv env e1' $ typeof (expToExpression env e)
impureExpToProgram env (Assign e@(FnCall (Var (Id "at" _) _) [e1, _] _) JustAssign
                               f _)
  = (env', R.Assign (expToExpression env' e) f')
   -- Propagate type of RHS to LHS.
   where env' | VoidType <- typeof e' = fixupEnv env e1 $ typeof f'
              | otherwise = env
         e'   = expToExpression env e
         f' | VoidType <- typeof e' = expToExpression env f
            | otherwise = expToExpression' env' (Just $ typeof e') f
impureExpToProgram env (Assign e JustAssign
                               f@(FnCall (Var (Id "at" _) _) [e1', _] _) _)
  = (env', R.Assign e' (expToExpression env' f))
   -- Propagate type of LHS to RHS.
   where env' | VoidType <- typeof f' = fixupEnv env e1' $ typeof e'
              | otherwise = env
         e' | VoidType <- typeof f' = expToExpression env e
            | otherwise = expToExpression' env' (Just $ typeof f') e
         f' = expToExpression env f
impureExpToProgram env (Assign e1 JustAssign e2 _)
  = (env, R.Assign (expToExpression env e1) (expToExpression env e2))
impureExpToProgram _ e = error $ "impureExpToProgram: " ++ show e

varToVariable :: TPEnv -> Exp -> Variable
varToVariable env (Var name _) = nameToVariable env name

nameToVariable :: TPEnv -> Id -> Variable
nameToVariable env name
 | Just v <- lookup (unId name) (vars env) = v
 | take 4 (unId name) == "task" = Variable fakeType (unId name) -- fake tasks
 | otherwise = error $ "varToVariable: Could not find: " ++ show name

expToExpression :: TPEnv -> Exp -> Expression
expToExpression env = expToExpression' env Nothing

-- No concept of Bool constants in the C parser. They appear at expresion
-- positions in our context.
expToExpression' :: TPEnv -> Maybe R.Type -> Exp -> Expression
expToExpression' _ _ (Var n _)
  | unId n == "true" = litB True
  | unId n == "false" = litB False
  | unId n == "NULL"  = litI32 0 -- Only representation for NULL in Program.
expToExpression' env _ v@Var{} = VarExpr $ varToVariable env v
expToExpression' _ _ (Const c _) = ConstExpr (constToConstant c)
expToExpression' env t (BinOp op e1 e2 _) = opToFunctionCall parms op
  where parms = map (expToExpression' env t) [e1, e2]
expToExpression' env t (UnOp op e _) = unOpToExp (expToExpression' env t e) op
expToExpression' _ _ (Assign e1 JustAssign e2 _) = error $ "Assign unimplemented" ++ show e1 ++ " = " ++ show e2
expToExpression' _ _ a@Assign{} = error ("AssignOp unhandled: " ++ show a)
expToExpression' _ _ PreInc{} = error "expToExpression: No support for preinc."
expToExpression' _ _ PostInc{} = error "expToExpression: No support for postinc."
expToExpression' _ _ PreDec{} = error "expToExpression: No support for predec."
expToExpression' _ _ PostDec{} = error "expToExpression: No support for postdec."
expToExpression' _ _ SizeofExp{} = error "expToExpression: No support for sizeof exp"
expToExpression' env _ (SizeofType t _) = R.SizeOf $ typToType env t
expToExpression' env _ (Cast t e _)
  = R.Cast (typToType env t) (expToExpression env e)
expToExpression' _ _ (Cond _c _e1 _e2 _) = error "expToExpression: No support for conditional statements."
expToExpression' env _ (Member e name _)
  = StructField (expToExpression env e) (unId name)
expToExpression' _ _ PtrMember{} = error "expToExpression: No support for ptrmember."
expToExpression' env _ (Index e1 e2 _)
  = ArrayElem (expToExpression env e1) [expToExpression env e2]
expToExpression' env tcontext (FnCall (Var (Id "at" _) _) [e1, e2] _)
  = ArrayElem (expToExpression env' e1) [expToExpression env' e2]
   where env' | (Var name _) <- e1
              , Just t <- tcontext
              , Just (Variable (ArrayType _ _ VoidType) _) <- lookup (unId name) (vars env)
              = fixupEnv env e1 t
              | otherwise = env
expToExpression' env _ (FnCall e es _)
  = expToFunctionCall env (map (expToExpression env) es) e
expToExpression' _ _ CudaCall{} = error "expToExpression: No support for CUDA."
expToExpression' _ _ Seq{} = error "expToExpression: No support for seq."
expToExpression' env _ (CompoundLit t ls _) = ConstExpr $ ArrayConst cs' $ typToType env t
  where cs = map (\(_, ExpInitializer (Const c _) _) -> constToConstant c) ls
        cs' = map (castConstant (typToType env t)) cs
expToExpression' _ _ StmExpr{} = error "expToExpression: No support for Stmexpr."
expToExpression' _ _ BuiltinVaArg{} = error "expToExpression: varargs not supported."
expToExpression' _ _ BlockLit{} = error "expToExpression: No support for blocklit."
expToExpression' _ _ e = error $ "expToExpression: Unhandled construct: " ++ show e

opToFunctionCall :: [Expression] -> BinOp -> Expression
opToFunctionCall es op = case opToString op of
                      Right s -> fun t s es
                      Left s -> fun (1 :# BoolType) s es
  where t = typeof (head es)

opToString :: BinOp -> Either String String
opToString Add = Right "+"
opToString Sub = Right "-"
opToString Mul = Right "*"
opToString Div = Right "/"
opToString Mod = Right "%"
opToString Eq = Left "=="
opToString Ne = Left "!="
opToString Lt = Left "<"
opToString Gt = Left ">"
opToString Le = Left "<="
opToString Ge = Left ">="
opToString Land = Left "&&"
opToString Lor = Left "||"
opToString And = Right "&"
opToString Or = Right "|"
opToString Xor = Right "^"
opToString Lsh = Right "<<"
opToString Rsh = Right ">>"

expToFunctionCall :: TPEnv -> [Expression] -> Exp -> Expression
expToFunctionCall _ es (Var name _)
  | Just v <- lookup (unId name) builtins
  = fun (varType v) (varName v) es
  | otherwise = fun fakeType (unId name) es

-- Signed integers are the default for literals, but that is not always
-- convenient. Fix things up afterwards instead.
castConstant :: R.Type -> Constant -> Constant
castConstant (1 :# t) (R.IntConst i _) = R.IntConst i t
castConstant _ c = error ("castConstant: Unexpected argument: " ++ show c)

constToConstant :: Const -> Constant
constToConstant (IntConst _ sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S32)
constToConstant (LongIntConst _ sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S64)
constToConstant (LongLongIntConst _ sgn i _)
  = R.IntConst i (NumType (signToSign sgn) S64)
constToConstant (FloatConst _ r _) = R.FloatConst r
constToConstant (DoubleConst _ r _) = R.DoubleConst r
constToConstant (LongDoubleConst _ r _) = R.DoubleConst r
constToConstant (CharConst _ _c _)
  = error "constToConstant: No support for character constants."
constToConstant (StringConst _ s _)  = R.StringConst s
constToConstant e = error ("constToConstant: Unhandled construct: " ++ show e)

declSpecToType :: TPEnv -> DeclSpec -> R.Type
declSpecToType env (DeclSpec _ _ ts _) = typSpecToType env ts
declSpecToType _ e = error ("declSpecToType: Unhandled construct: " ++ show e)

initToFunDecl :: TPEnv -> DeclSpec -> Init -> [Param] -> (TPEnv, Entity)
initToFunDecl env ds is ps = (env', Proc iv False ps' VoidType Nothing)
 where (iv, _, _)  = initToName env (declSpecToType env' ds) is
       (env', ps') = mapAccumL paramToVariable env ps

initToNames :: TPEnv -> DeclSpec -> [Init] -> (TPEnv, [Declaration])
initToNames env ds is = (updateEnv env vs, dv)
 where ivs = map (initToName env (declSpecToType env ds)) is
       vs  = map (\(name, t, _) -> Variable t name) ivs
       dv  = zipWith (\v (_, _, start) -> Declaration v start) vs ivs

initToName :: TPEnv -> R.Type -> Init -> (String, R.Type, Maybe Expression)
initToName env tp (Init name dcl _ ints _ _)
  = (unId name, t, maybe Nothing (initializerToExp env t) ints)
    where t = declToType tp dcl

initializerToExp :: TPEnv -> R.Type -> Initializer -> Maybe Expression
initializerToExp env _ (ExpInitializer e _)
  = Just $ expToExpression env e
initializerToExp env t@(StructType _ ts) (CompoundInitializer es _)
  | isCompoundZero es
  = Nothing
  | otherwise
  = Just (ConstExpr (StructConst (zipWith (structInit env) ts es) t))
initializerToExp _ t e
  = error $ "initializerToExp: Unexpected argument: " ++ show e ++ " of type " ++ show t

structInit :: TPEnv -> (String, R.Type)
           -> (Maybe Designation, Initializer) -> (Maybe String, Constant)
structInit env (_, t) (d, e)
  | Just (ConstExpr c) <- e'
  = (maybe Nothing (\(Designation ((MemberDesignator i _):_) _) -> Just $ unId i) d, c)
  | otherwise
  = error $ "structInit: Unexpected pattern : " ++ show (d, e)
   where e' = initializerToExp env t e

isCompoundZero :: [(Maybe Designation, Initializer)] -> Bool
isCompoundZero [(_, ExpInitializer (Const (IntConst _ _ 0 _) _) _)] = True
isCompoundZero _ = False

unOpToExp :: Expression -> UnOp -> Expression
unOpToExp e AddrOf = R.AddrOf e
unOpToExp e Deref  = R.Deref e
unOpToExp (ConstExpr (R.IntConst n t)) Negate
  = ConstExpr (R.IntConst (-1*n) t)
unOpToExp (ConstExpr (R.FloatConst  n)) Negate
  = ConstExpr (R.FloatConst (-1*n))
unOpToExp (ConstExpr (R.DoubleConst n)) Negate
  = ConstExpr (R.DoubleConst (-1*n))
unOpToExp e Negate = fun (typeof e) "-" [e]
unOpToExp e Positive = e
unOpToExp _ Not = error "Not"
unOpToExp e Lnot = fun (1 :# BoolType) "!" [e]

typToType :: TPEnv -> Type -> R.Type
typToType env (Type ds _ _) = declSpecToType env ds
typToType _ t = error $ "typToType: Unhandled construct: " ++ show t

typSpecToType :: TPEnv -> TypeSpec -> R.Type
typSpecToType _ Tvoid{} = VoidType
typSpecToType _ Tchar{} = error "Tchar"
typSpecToType _ Tshort{} = error "TShort"
typSpecToType _ (Tint Nothing _) = 1 :# NumType R.Unsigned S32
typSpecToType _ (Tint _ _)       = 1 :# NumType R.Signed S32
typSpecToType _ Tlong{} = error "Tlong"
typSpecToType _ Tlong_long{} = error "longlong"
typSpecToType _ Tfloat{}         = 1 :# FloatType
typSpecToType _ Tdouble{}        = 1 :# DoubleType
typSpecToType _ Tlong_double{} = error "long double"
-- Anonymous typedefs.
typSpecToType env (Tstruct Nothing mfg _ _)
  = StructType fakeName (concatMap (fieldGroupToType env) (fromJust mfg))
-- Array types are incomplete so fake one. We recover the type elsewhere.
typSpecToType _ (Tstruct (Just (Id "array" _)) _ _ _)
  = ArrayType Global universal fakeType
typSpecToType _ (Tstruct (Just (Id s _)) _ _ _)
  | "awl" `isPrefixOf` s
  , [t] <- decodeType s
  = t
-- Ivars are declared externally so just fake a type.
typSpecToType _ (Tstruct (Just (Id "ivar" _)) Nothing _ _)
  = IVarType fakeType
-- Called for both declaration and use-sites.
typSpecToType env t'@(Tstruct (Just (Id s _)) mfg _ _)
  | Just t <- findBuiltinDeclaration env s = t
  | Just t <- findLocalDeclaration env s = t
  | Nothing <- mfg = error ("typSpecToType: Internal error: " ++ show t'
                           ++ " not found in:\n" ++ show (headerDefs env)
                           ++ " and not in " ++ show (typedefs env))
  | otherwise = StructType s (concatMap (fieldGroupToType env) (fromJust mfg))
typSpecToType _ Tunion{} = error "typSpecToType: No support for union."
typSpecToType _ Tenum{} = error "typSpecToType: No support for enum."
typSpecToType env (Tnamed i@Id{} _ _) = namedToType env (unId i)
typSpecToType _ TtypeofExp{} = error "typSpecToType: No support for typeofExp"
typSpecToType _ TtypeofType{} = error "typSpecToType: No support for typeofType"
typSpecToType _ Tva_list{} = error "typSpecToType: No support for valist."
typSpecToType _ t = error $ "typSpecToType: Unknown type " ++ show t

namedToType :: TPEnv -> String -> R.Type
namedToType _ "uint64_t" = 1 :# NumType R.Unsigned S64
namedToType _ "uint40_t" = 1 :# NumType R.Unsigned S40
namedToType _ "uint32_t" = 1 :# NumType R.Unsigned S32
namedToType _ "uint16_t" = 1 :# NumType R.Unsigned S16
namedToType _ "uint8_t"  = 1 :# NumType R.Unsigned S8
namedToType _ "int64_t"  = 1 :# NumType R.Signed S64
namedToType _ "int40_t"  = 1 :# NumType R.Signed S40
namedToType _ "int32_t"  = 1 :# NumType R.Signed S32
namedToType _ "int16_t"  = 1 :# NumType R.Signed S16
namedToType _ "int8_t"   = 1 :# NumType R.Signed S8
namedToType _ "bool"     = 1 :# BoolType
namedToType env s | Just t <- findLocalDeclaration env s = t
namedToType _ s          = error $ "namedToType: Unrecognized type: " ++ s

declToType :: R.Type -> Decl -> R.Type
declToType t DeclRoot{} = t
-- Truncate one level of pointers on array types.
declToType t (Ptr _ DeclRoot{} _) | pointedArray t = t
declToType t (Ptr _ dcl _)  = declToType (1 :# Pointer t) dcl
declToType _ BlockPtr{} = error "Blocks?"
declToType t (Array _ (NoArraySize _) _ _) = NativeArray Global Nothing t
declToType _ (Array _tqs _sz _dcl _)
  = error "declToType: No support for sized native arrays yet."
declToType t (Proto _ params _)
  = trace ("DEBUG: Proto: " ++ show params) t
declToType _ (OldProto _dcl _ids _) = error "OldProto"
declToType _ d = error $ "declToType: Unhandled construct: " ++ show d

pointedArray :: R.Type -> Bool
pointedArray ArrayType{}                   = True
pointedArray (_ :# (Pointer t))            = pointedArray t
pointedArray _                             = False

fieldGroupToType :: TPEnv -> FieldGroup -> [(String, R.Type)]
fieldGroupToType env (FieldGroup dss fs _)
  = zip (map fieldToName fs) $ replicate (length fs) $ declSpecToType env dss
fieldGroupToType _ g
  = error $ "fieldGroupToType: Unhandled construct: " ++ show g

signToSign :: Signed -> Signedness
signToSign Signed   = R.Signed
signToSign Unsigned = R.Unsigned

fieldToName :: Field -> String
fieldToName (Field (Just s) _ _ _) = unId s

typeDefToName :: Typedef -> String
typeDefToName (Typedef s _ _ _) = unId s

unId :: Id -> String
unId (Id n _) = n
unId i = error ("unId: Unhandled construct: " ++ show i)

-- Some place holders.
fakeType :: R.Type
fakeType = VoidType

fakeName :: String
fakeName = "fakeName"

-- Feldspar "builtins".
builtins :: [(String, R.Variable)]
builtins =
  [ ("getLength", Variable (1 :# NumType R.Unsigned S32) "getLength")
  ]

findBuiltinDeclaration :: TPEnv -> String -> Maybe R.Type
findBuiltinDeclaration env = go (headerDefs env)
  where go [] _ = Nothing
        go ((_, _, StructDef n fs):t) s
         | n == s = Just (StructType n (map toType fs))
         | otherwise = go t s
        go e _ = error $ "findBuiltinDeclaration: Non-Struct found: " ++ show e
        toType (StructMember n t) = (n, t)

findLocalDeclaration :: TPEnv -> String -> Maybe R.Type
findLocalDeclaration env = go (typedefs env)
  where go [] _ = Nothing
        go (tp@(StructType n _):t) s | n == s = Just tp
                                     | otherwise = go t s
        go e _ = error $ "findLocalDeclaration: Non-Struct found: " ++ show e

-- | Environments.
data TPEnv = TPEnv
    { vars :: [(String, Variable)]
    , typedefs :: [R.Type]
    , headerDefs :: [(String, [R.Type], Entity)]
    } deriving Show

emptyEnv :: [(String, [R.Type], Entity)] -> TPEnv
emptyEnv = TPEnv [] []

updateEnv :: TPEnv -> [R.Variable] -> TPEnv
updateEnv env ns = env { vars = nt ++ vars env }
     where nt = map (\v@(Variable _ name) -> (name, v)) ns

updateEnv2 :: TPEnv -> [R.Variable] -> R.Type -> TPEnv
updateEnv2 (TPEnv vs tdefs hdefs) ns t
 = TPEnv (nt ++ vs) (t:tdefs) hdefs
     where nt = map (\v@(Variable _ name) -> (name, v)) ns

-- Patch the type information in the environment when we learn more.
fixupEnv :: TPEnv -> Exp -> R.Type -> TPEnv
fixupEnv env _ VoidType = env -- No new type information.
fixupEnv env (UnOp Deref (Var (Id s _) _) _) tp = env { vars = goVar (vars env) }
  where goVar [] = []
        goVar ((n, Variable (l :# (Pointer (ArrayType as r _))) n'):t)
         | s == n = (n, Variable (l :# Pointer (ArrayType as r tp)) n'):goVar t
        goVar (p:t) = p:goVar t
fixupEnv _ (UnOp Deref e _) _ = error $ "fixupEnv: No support for " ++ show e
fixupEnv env (Var (Id s _) _) tp = env { vars = goVar (vars env) }
  where goVar [] = []
        goVar ((n, Variable (ArrayType as r _) n'):t)
         | s == n = (n, Variable (ArrayType as r tp) n'):goVar t
        goVar (p:t) = p:goVar t
fixupEnv env (FnCall (Var (Id "at" _) _) [(UnOp Deref (Var (Id s _) _) _), _] _) tp = env { vars = goVar (vars env) }
  where goVar [] = []
        goVar ((n, Variable (k :# (Pointer (ArrayType as1 r1 (ArrayType as2 r2 _)))) n'):t)
         | s == n = let nt = (k :# Pointer (ArrayType as1 r1 (ArrayType as2 r2 tp)))
                    in (n, Variable nt n'):goVar t
        goVar (p:t) = p:goVar t
fixupEnv env (Member (Var (Id s _) _) (Id name _) _) tp = env { vars = goStruct (vars env) }
  where goStruct [] = []
        goStruct ((n, Variable (StructType s' ns) n'):t)
         | s == n
         , Just _ <- lookup name ns
         = (n, Variable (StructType s' ns') n'):goStruct t
           where ns' = map structFixup ns
                 structFixup (mem, ArrayType as r _)
                  | mem == name = (mem, ArrayType as r tp)
                 structFixup (mem, _)
                  | mem == name = (mem, tp)
                 structFixup e = e
        goStruct (p:t) = p:goStruct t
fixupEnv env (Member (FnCall (Var (Id "at" _) _) [Var (Id s _) _, _] _) (Id name _) _) tp = env { vars = goStructAt (vars env) }
  where goStructAt [] = []
        goStructAt ((n, Variable (StructType s' ns) n'):t)
         | s == n
         , Just _ <- lookup name ns
         = (n, Variable (StructType s' ns') n'):goStructAt t
           where ns' = map structFixup ns
                 structFixup (mem, ArrayType as r _)
                  | mem == name = (mem, ArrayType as r tp)
                 structFixup e = e
        goStructAt (p:t) = p:goStructAt t
fixupEnv env _ _ = env

-- Expression builders.

-- | Build an expression that adds one
plusOne :: Expression -> Expression
plusOne e = opToFunctionCall [e, litI32 1] Add

-- Misc helpers

patchHdefs :: [Entity] -> [(String, [R.Type], Entity)]
patchHdefs [] = []
patchHdefs (StructDef s _:t)
  = (s, map snd ts, StructDef s $ map toDef ts):patchHdefs t
  where [StructType _ ts] = decodeType s
        toDef (n,t') = StructMember n t'
patchHdefs (p@(Proc n _ ins _ Nothing):t)
  = (n, map typeof ins, p):patchHdefs t
patchHdefs (_:t) = patchHdefs t

mkDef :: R.Type -> [Entity]
mkDef (StructType n fields)
  = [StructDef n (map (\(n', t) -> StructMember n' t) fields)]
-- Only interested in struct definitions so discard everything else.
mkDef _ = []

-- | Lookup for triples
lookup3 :: String -> [(String, [R.Type], Entity)] -> [R.Type]
lookup3 s xs = ts
  where (_, ts, _) = head $ filter (\(s1,_,_) -> s1 == s) xs

-- Input helpers.

-- FIXME: We have removed the at macro, clean this up.

-- The C parser chokes when parsing a function call that has a type
-- parameter as first argument. This is precisely what we have with our
-- "at" macro. We can reconstruct the first argument by other means, so
-- just change all calls on the form "at(type,p1,p2)" to "at(p1, p2)".
massageInput:: B.ByteString -> B.ByteString
massageInput xs' = foldr (\w xs -> dropBitMask xs w) tmp otherWords'
 where -- Drop first parameter for these functions.
       prefixWords = map B.pack ["at(", "ivar_put(","ivar_get(", "ivar_get_nontask("]
       -- Drop some parameters according to mask for these.
       otherWords = [ ("spawn2(", [True, False, True, False, True])
                    , ("run2(", [True, False, False])
                    , ("spawn3(", [True, False, True, False, True
                                       , False, True])
                    , ("run3(", [True, False, False, False])
                    , ("spawn4(", [True, False, True, False, True
                                       , False, True, False, True])
                    , ("run4(", [True, False, False, False, False])
                    , ("spawn5(", [True, False, True, False, True
                                       , False, True, False, True
                                       , False, True])
                    , ("run5(", [True, False, False, False, False,False])]
       otherWords' = map (\(p1, b) -> (B.pack p1, b)) otherWords
       tmp = foldr (\w xs -> dropFirstArg xs w) xs' prefixWords

dropFirstArg:: B.ByteString -> B.ByteString -> B.ByteString
dropFirstArg xs wrd = go (B.breakSubstring wrd xs) []
  where go (h, t) acc
          | B.null t = B.append (B.concat $ reverse acc) h
          | otherwise = go (B.breakSubstring wrd $ fixArg t) (wrd:h:acc)
        -- Drops the prefix "<word>.*,".
        fixArg = B.drop 1 . B.dropWhile (/= ',') . B.drop (B.length wrd)

dropBitMask :: B.ByteString -> (B.ByteString, [Bool]) -> B.ByteString
dropBitMask xs (wrd, bs) = go (B.breakSubstring wrd xs) []
  where comma = B.pack ","
        rparen = B.pack ")"
        go (h, t) acc
          | B.null t = B.append (B.concat $ reverse acc) h
          | otherwise = go (B.breakSubstring wrd rest) (rparen:as:wrd:h:acc)
             where (as, rest) = fixArg (B.span (/= ';') $ B.drop (B.length wrd) t)
        -- Drops the types.
        fixArg (as, rest) = (flt $ zip bs (B.split ',' $ B.init as), rest)
          where flt = B.concat  . intersperse comma . map snd . filter fst
