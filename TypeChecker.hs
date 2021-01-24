module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

import Control.Monad

bTy          = [Type_bool]
numTy        = [Type_int,Type_double]
numStrTy     = [Type_int,Type_double,Type_string]
numBTy       = [Type_bool,Type_int,Type_double]

inferExp :: (Monad m) => Env -> Exp -> m Exp
inferExp _  ETrue      	    = return (ETyped ETrue        Type_bool)
inferExp _  EFalse     	    = return (ETyped EFalse       Type_bool)
inferExp _  (EInt n)   	    = return (ETyped (EInt n)     Type_int)
inferExp _  (EDouble n)	    = return (ETyped (EDouble n)  Type_double)
inferExp _  (EString s)	    = return (ETyped (EString s)  Type_string)
inferExp e  (EId id) 	    = 
         do t <- lookupVar e id
            return (ETyped (EId id) t)
inferExp e  (EApp id exps)   =
	 do (ts,t) <- lookupFun e id
            if (length ts /= length exps) then
              fail "Not enough arguments"  -- bad19.cc
            else do
              expsty <- mapM (\ (exp,ty) -> checkExp e exp ty) (zip exps ts)
              return (ETyped (EApp id expsty) t) 
inferExp e (EPIncr i)       = unaryExp e numTy   EPIncr i
inferExp e (EPDecr i)       = unaryExp e numTy   EPDecr i 
inferExp e (EIncr  i)       = unaryExp e numTy   EIncr  i 
inferExp e (EDecr  i)       = unaryExp e numTy   EDecr  i 
inferExp e (ETimes e1 e2)   = binExp e numTy     ETimes e1 e2 id
inferExp e (EPlus  e1 e2)   = binExp e numStrTy  EPlus  e1 e2 id
inferExp e (EDiv   e1 e2)   = binExp e numTy     EDiv   e1 e2 id
inferExp e (EMinus e1 e2)   = binExp e numTy     EMinus e1 e2 id
inferExp e (ELt    e1 e2)   = binExp e numTy     ELt    e1 e2 (const Type_bool)
inferExp e (EGt    e1 e2)   = binExp e numTy     EGt    e1 e2 (const Type_bool)
inferExp e (ELtEq  e1 e2)   = binExp e numTy     ELtEq  e1 e2 (const Type_bool)
inferExp e (EGtEq  e1 e2)   = binExp e numTy     EGtEq  e1 e2 (const Type_bool)
inferExp e (EEq    e1 e2)   = binExp e numBTy    EEq    e1 e2 (const Type_bool)
inferExp e (ENEq   e1 e2)   = binExp e numBTy    ENEq   e1 e2 (const Type_bool)
inferExp e (EAnd   e1 e2)   = binExp e bTy       EAnd   e1 e2 (const Type_bool)
inferExp e (EOr    e1 e2)   = binExp e bTy       EOr    e1 e2 (const Type_bool)
inferExp e (EAss   i  e1)   = 
         do t     <- lookupVar e i
            e1ty  <- checkExp e e1 t
            return (ETyped (EAss i e1ty) t)

unaryExp :: (Monad m) => Env -> [Type] -> (Id -> Exp) -> Id -> m Exp
unaryExp env typs ec i     = 
         do t               <- lookupVar env i
            if (t `elem` typs) then
               return (ETyped (ec i) t)
            else
               fail "Unary incr/decremental operator not applied to a numeric expression"

binExp :: (Monad m) => Env -> [Type] -> (Exp -> Exp -> Exp) -> Exp -> Exp -> (Type -> Type) -> m Exp
binExp env typs ec e1 e2 f = 
         do (ETyped e1' t1) <- inferExp env e1
            (ETyped e2' t2) <- inferExp env e2
            if (t1  == t2) then
                 return (ETyped (ec (ETyped e1' t1) (ETyped e2' t2)) (f t2)) 
            else
                fail ("Arithmetic/string/bool operator "            ++ 
                     (head . words . show) (ec (EInt 1) (EInt 1))  ++ 
                     "not appied to arguments of the same type"             )

checkExp :: (Monad m) => Env -> Exp -> Type -> m Exp
checkExp e exp t =
	 do (ETyped e' t') <- inferExp e exp
	    if (t' == t) then
	       return (ETyped e' t')
	    else
	       fail $ "type of "   ++ printTree exp ++ "\n" ++
	              "expected "  ++ printTree t   ++ "\n" ++
     	              "but found " ++ printTree t' 
	
checkStm :: (Monad m) => Type -> Env -> Stm -> m (Env,Stm)
checkStm _ e (SExp exp)          = do expt <- inferExp e exp
                                      return  (e,SExp expt)
checkStm _ e (SDecls t ids)      = liftM (\ e' -> (e' , SDecls t ids))
                                         (foldM (\ e' id -> updateVar e' id t) e ids)
checkStm _ e (SInit t id exp)    = do e' <- updateVar e id t
                                      expt <- checkExp e' exp t  
                                      return (e', SInit t id expt)
checkStm ty e (SReturn exp)      = do expt <- checkExp e exp ty
                                      return (e, SReturn expt)
checkStm ty e SReturnVoid        = if (ty == Type_void) then
                                      return (e, SReturnVoid)
                                   else 
                                      fail ("Return type : void" ++ "\nExpected type :" ++ show ty)
checkStm ty e (SWhile  exp s)    = do expt <-  checkExp e exp Type_bool
                                      (e',st) <- checkStm ty e s
				      return (e' , SWhile expt st)
checkStm ty e (SBlock stms)      = do stmst <- checkStms ty (newBlock e) stms
                                      return (e , SBlock stmst)
checkStm ty e (SIfElse exp s1 s2)= do expt      <- checkExp e exp Type_bool
                                      (_, s1t ) <- checkStm ty e s1
                                      (_, s2t ) <- checkStm ty e s2
				      return (e,SIfElse expt s1t s2t)

checkStms :: (Monad m) => Type -> Env -> [Stm] -> m [Stm]
checkStms ty e stmts = liftM (reverse.snd)
                             (foldM (\ (e' , ss) s' -> do {(e'', s'') <- checkStm ty e' s'; return (e'',s'':ss)})
		                    (e,[])
			  	    stmts)

args2P :: [Arg] -> [(Id,Type)]
args2P = map (\ (ADecl ty id) -> (id,ty))

def2sig :: Program -> [(Id, ([Type],Type))]
def2sig (PDefs ds) = map (\ (DFun ty id args _) -> (id,(map (\ (ADecl t i) -> t) args,ty))) ds

checkFunsBodies :: (Monad m) => Env -> Program -> m [Def]
checkFunsBodies e (PDefs defs)
  = mapM (\ (DFun ty id args stms) -> do e'    <- updateVars e (args2P args)
                                         stmst <- checkStms ty e' stms
					 return (DFun ty id args stmst)     )
	 defs

buildInFunctions :: [(Id , ([Type],Type))]
buildInFunctions = [
  (Id "printString"  , ([Type_string],Type_void)),
  (Id "printInt"     , ([Type_int]   ,Type_void)),
  (Id "printDouble"  , ([Type_double],Type_void)),
  (Id "readInt"      , ([]           ,Type_int)),
  (Id "readDouble"   , ([]           ,Type_double)),
  (Id "readString"   , ([]           ,Type_string)),
  (Id "d2Str"        , ([Type_double],Type_string)),
  (Id "i2Str"        , ([Type_int]   ,Type_string)),
  (Id "plusString"   , ([Type_string,Type_string],Type_string))
  ]

typecheck :: (Monad m) => Program -> m Program
typecheck p =
	  do e <- updateFuns (buildInFunctions ++ (def2sig p))
	     defs <- checkFunsBodies e p
	     return (PDefs defs)

