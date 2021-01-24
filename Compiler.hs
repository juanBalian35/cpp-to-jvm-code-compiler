module Compiler where

import AbsCPP
import TypeChecker

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.State.Lazy

type Instruction = String
type FunType     = String
type Label       = Int

data Env = Env {
  funs      :: Map.Map Id FunType,
  vars      :: [Map.Map Id Int],    -- Stack of variables blocks !
  maxvars   :: [Int],               -- Stack of counter for variable addresses in each block
  maxstk    :: Int,                 -- counter for maximal stack depth
  labels    :: Int,                 -- counter for jump labels
  code      :: [Instruction]
}

emptyEnv :: Env
emptyEnv = Env { funs=Map.empty,
                 vars=[Map.empty],
                 maxvars=[0],
                 maxstk=0,
                 labels=0,
                 code=[]}

emit :: Instruction -> State Env ()
emit i = do
  env <- get
  put (env { code = i : code env })

typeSize :: Type -> Int
typeSize Type_double = 2
typeSize _           = 1

extendVar :: Type -> Id -> State Env ()
extendVar t i = do
  env <- get
  put (env { vars    = Map.insert i (head (maxvars env)) (head (vars env)) : tail (vars env)
           , maxvars = head (maxvars env) + (typeSize t) : tail (maxvars env)
           }
      )

extendVars :: Type -> [Id] -> State Env ()
extendVars t ids = mapM_ (\x -> extendVar t x) ids

typeJVM :: Type -> String
typeJVM Type_int    = "I"
typeJVM Type_double = "D"
typeJVM Type_void   = "V"
typeJVM Type_bool   = "Z"
typeJVM Type_string = "Ljava/lang/String;"

funJVMType :: String -> [Type] -> Type -> FunType
funJVMType i typs rty = i ++ "(" ++ (foldr (\ t s -> typeJVM t ++ s) "" typs) ++ ")" ++ typeJVM rty

funJVM :: String -> String -> [Type] -> Type -> FunType
funJVM i clas argty rty = "invokestatic " ++ clas ++"/" ++ funJVMType i argty rty

extendFunEnv :: Id -> FunType -> State Env ()
extendFunEnv f t = do
  env <- get
  put (env { funs = Map.insert f t (funs env) })

extendDef :: String -> Def -> State Env ()
extendDef cls (DFun t (Id i) args _) = extendFunEnv (Id i) (funJVMType i (map aux args) t)
  where
    aux (ADecl t _) = t

-- Besides added in type checker
extendBuiltinDefs :: State Env ()
extendBuiltinDefs = mapM_ ( \ ((Id i),(argTys,rty)) -> extendFunEnv (Id i) $ funJVM i "Runtime" argTys rty) buildInFunctions

newBlock :: State Env ()
newBlock = do
  env <- get
  put (env { vars    = Map.empty : vars env
           , maxvars = head (maxvars env) : maxvars env
           }
      )

exitBlock :: State Env ()
exitBlock = do
  env <- get
  put (env { vars    = tail (vars env)
           , maxvars = tail (maxvars env)
           }
      )

newLabel :: State Env String
newLabel  = do
  env <- get
  put (env { labels = labels env + 1 })
  return ("label" ++ show (labels env))

lookupFun :: Id -> State Env FunType
lookupFun f = do
  env <- get
  return ((funs env) Map.! f)

lookupVars :: Id -> [Map.Map Id Int] -> Int
lookupVars i []     = error "Error, are variables being loaded correctly?"
lookupVars i (m:ms) = case Map.lookup i m of
                          Nothing -> lookupVars i ms;
                          Just d -> d;

lookupVar :: Id -> State Env Int
lookupVar i = do
  env <- get
  return (lookupVars i (vars env))

-- Entry point from ccpp.
-- Arguments: cls is the class name and p is the typed embedded abstract syntax tree (returned by the type checker).
compile :: String -> Program -> [Instruction]
compile cls p = reverse (code (execState (compileP cls p) emptyEnv))

compileP :: String -> Program -> State Env ()
compileP cls (PDefs defs) = do
  emit $ ".class public " ++ cls
  emit $ ".super java/lang/Object"
  emit $ ""
  emit $ ".method public <init>()V"
  emit $ "  aload_0"
  emit $ "  invokenonvirtual java/lang/Object/<init>()V"
  emit $ "  return"
  emit $ ".end method"
  emit $ ""
  extendBuiltinDefs
  mapM  (extendDef cls) defs
  mapM_ compileDef defs

compileDef :: Def -> State Env ()
compileDef (DFun t (Id i) args stmts) = do
  newBlock
  if i == "main" then do
       emit $ ".method public static main([Ljava/lang/String;)V"
       extendVar Type_string (Id "args") -- in fact is an array of strings ([Ljava/lang/String;)
  else emit $ ".method public static " ++ (funJVMType i (map (\ (ADecl t _) -> t) args) t)
  emit $ ".limit locals 1000"  -- aca se puede ser mas precisoo !
  emit $ ".limit stack  1000"  -- aca se puede ser mas precisoo !
  mapM (\ (ADecl t i) -> extendVar t i) args
  mapM compileStm stmts
  exitBlock
  emit $ "return"
  emit $ ".end method"
  emit ""

compileStm :: Stm -> State Env ()
compileStm (SDecls t ids) = extendVars t ids
compileStm (SInit t i e) = do
  extendVar t i
  compileExp e
  lookf <- lookupVar i
  if t == Type_string
    then emit ("astore " ++ show lookf)
    else if t == Type_double
        then emit ("dstore " ++ show lookf)
        else emit ("istore " ++ show lookf)
compileStm (SExp e@(ETyped _ t)) = do
  compileExp e
  if (elem t [Type_int, Type_bool, Type_string])
    then emit "pop"
    else if t == Type_double
      then emit "pop2"
      else return ()
compileStm (SWhile e s) = do
  normal <- newLabel
  out <- newLabel
  emit(normal ++ ":")
  compileExp e
  emit("ifeq " ++ out)
  compileStm s
  emit("goto " ++ normal)
  emit(out ++ ":")
compileStm (SBlock stms) = do
  newBlock
  mapM_ compileStm stms
  exitBlock
compileStm (SReturn e@(ETyped _ t)) = do
  compileExp e
  if t == Type_double
    then do emit "dreturn"
    else do emit "ireturn"

compileStm SReturnVoid = emit "return"

compileStm (SIfElse e st sf) = do
  true <- newLabel
  false <- newLabel
  compileExp e
  emit("ifeq " ++ false)
  compileStm st
  emit("goto " ++ true)
  emit(false ++ ":")
  compileStm sf
  emit(true ++ ":")

compileExp :: Exp -> State Env ()
compileExp (ETyped ETrue _) = emit "bipush 1"
compileExp (ETyped EFalse _) = emit "bipush 0"
compileExp (ETyped (EId x) t) = do
  lookf <- lookupVar x
  case t of
   Type_double -> emit("dload " ++ show lookf)
   Type_string -> emit("aload " ++ show lookf)
   _           -> emit("iload " ++ show lookf)
compileExp (ETyped (EApp f es) _) = do
  mapM_ compileExp es
  invf <- lookupFun f
  emit invf
compileExp (ETyped (EInt x) _) = emit ("ldc " ++ show x)
compileExp (ETyped (EDouble x) _) = emit ("ldc2_w " ++ show x)
compileExp (ETyped (EString x) _) = emit ("ldc " ++ x)
compileExp (ETyped (EAss i e) t) = do
  compileExp e
  lookf <- lookupVar i
  if t == Type_string
    then do
      emit "dup"
      emit ("astore " ++ show lookf)
    else if t == Type_double
        then do
           emit "dup2"
           emit ("dstore " ++ show lookf)
        else do
            emit "dup"
            emit ("istore " ++ show lookf)

compileExp (ETyped (EIncr i) Type_int) = do
  lookf <- lookupVar i
  emit("iinc " ++ show lookf ++ " 1")
  emit("iload " ++ show lookf)
compileExp (ETyped (EDecr i) Type_int) = do
  lookf <- lookupVar i
  emit("iinc " ++ show lookf ++ " -1")
  emit("iload " ++ show lookf)
compileExp (ETyped (EPIncr i) Type_int) = do
  lookf <- lookupVar i
  emit("iload " ++ show lookf)
  emit("iinc " ++ show lookf ++ " 1")
compileExp (ETyped (EPDecr i) Type_int) = do
  lookf <- lookupVar i
  emit("iload " ++ show lookf)
  emit("iinc " ++ show lookf ++ " -1")
compileExp (ETyped (EPlus e1 e2) Type_string) = do
  compileExp e1
  compileExp e2
  inv <- lookupFun (Id "concatStr")
  emit inv
compileExp (ETyped (EPlus e1 e2) t) = do
  compileExp e1
  compileExp e2
  if t == Type_int
    then emit "iadd"
    else emit "dadd"
compileExp (ETyped (ETimes e1 e2) t) = do
  compileExp e1
  compileExp e2
  if t == Type_int
    then emit "imul"
    else emit "dmul"
compileExp (ETyped (EDiv e1 e2) t) = do
  compileExp e1
  compileExp e2
  if t == Type_int
    then emit "idiv"
    else emit "ddiv"
compileExp (ETyped (EMinus e1 e2) t) = do
  compileExp e1
  compileExp e2
  if t == Type_int
    then emit "isub"
    else emit "dsub"

compileExp (ETyped (EAnd e1 e2) _) = do
  t <- newLabel
  f <- newLabel

  compileExp e1
  emit("ifeq " ++ f)
  compileExp e2
  emit("ifeq " ++ f)
  emit("bipush 1")
  emit("goto " ++ t)
  emit(f ++ ":")
  emit("bipush 0")
  emit(t ++ ":")

compileExp (ETyped (EOr e1 e2) _) = do
  true <- newLabel
  false <- newLabel
  compileExp e1
  emit("ifne " ++ true)
  compileExp e2
  emit("ifne " ++ true)
  emit("bipush 0")
  emit("goto " ++ false)
  emit(true ++ ":")
  emit "bipush 1"
  emit(false ++ ":")
compileExp (ETyped (ELt e1@(ETyped _ Type_int) e2) _) = compileExpCmp e1 e2 Lt
compileExp (ETyped (EGt e1@(ETyped _ Type_int) e2) _) = compileExpCmp e1 e2 Gt
compileExp (ETyped (ELtEq e1@(ETyped _ Type_int) e2) _) = compileExpCmp e1 e2 Le
compileExp (ETyped (EGtEq e1@(ETyped _ Type_int) e2) _) = compileExpCmp e1 e2 Ge
compileExp (ETyped (EEq e1@(ETyped _ Type_int) e2) _) = compileExpCmp e1 e2 Equal
compileExp (ETyped (ENEq e1@(ETyped _ Type_int) e2) _) = compileExpCmp e1 e2 NEqual
compileExp (ETyped (ELt e1@(ETyped _ Type_double) e2) _) = compileExpCmp e1 e2 Lt
compileExp (ETyped (EGt e1@(ETyped _ Type_double) e2) _) = compileExpCmp e1 e2 Lt
compileExp (ETyped (ELtEq e1@(ETyped _ Type_double) e2) _) = compileExpCmp e1 e2 Le
compileExp (ETyped (EGtEq e1@(ETyped _ Type_double) e2) _) = compileExpCmp e1 e2 Ge
compileExp (ETyped (EEq e1@(ETyped _ Type_double) e2) _) = compileExpCmp e1 e2 Equal
compileExp (ETyped (ENEq e1@(ETyped _ Type_double) e2) _) = compileExpCmp e1 e2 NEqual
compileExp exp = error ("Expression is not defined for expresion compiler" ++ show exp)

compileExpCmp :: Exp -> Exp -> Cmp -> State Env ()
compileExpCmp e1 e2 c = do
  true <- newLabel
  emit "bipush 1"
  compileExp e1
  compileExp e2
  emit ((show c) ++ true)
  emit "pop"
  emit "bipush 0"
  emit (true ++ ":")

compileExpCmpDouble :: Exp -> Exp -> Cmp -> State Env ()
compileExpCmpDouble e1 e2 c = do
  true <- newLabel
  emit "bipush 1"
  compileExp e1
  compileExp e2
  emit "dcmpcg"
  emit (showDbl c ++ true)
  emit "pop"
  emit "bipush 0"
  emit (true ++ ":")

data Cmp = Equal | NEqual | Lt | Gt | Ge | Le
  deriving (Eq)

instance Show Cmp where
  show Equal  = "if_icmpeq "
  show NEqual = "if_icmpne "
  show Lt     = "if_icmplt "
  show Gt     = "if_icmpgt "
  show Ge     = "if_icmpge "
  show Le     = "if_icmple "

showDbl :: Cmp -> Instruction
showDbl Equal  = "ifeq "
showDbl NEqual = "ifne "
showDbl Lt     = "iflt "
showDbl Gt     = "ifgt "
showDbl Ge     = "ifge "
showDbl Le     = "ifle "
