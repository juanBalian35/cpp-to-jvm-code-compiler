module Env where

import AbsCPP
import PrintCPP
import ErrM

import qualified Data.Map.Strict as Map

type Env = (Sig,[Context])
type Sig = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type

lookupVar :: (Monad m) =>  Env -> Id        -> m Type
lookupVar (s,[])   i = fail $ "Undefined variable:" ++ show i
lookupVar (s,c:cs) i = maybe (lookupVar (s,cs) i) return (Map.lookup i c)

lookupFun :: (Monad m) => Env -> Id                  -> m ([Type],Type)
lookupFun (s,c)    i 
  = maybe (fail  $ "Undefined function:" ++ show i) return (Map.lookup i s)

updateVar :: (Monad m) => Env -> Id -> Type          -> m Env
updateVar (s,c:cs) i t =
	  case Map.member i c of {
	       True  -> fail ("Variable redefinition:" ++ show i);
	       False -> return (s, (Map.insert i t c):cs)
	  }

updateVars :: (Monad m) => Env -> [(Id,Type)]        -> m Env
updateVars env = foldl (\ r (i,t) -> r >>= \ e -> updateVar e i t) (return env)

updateFun :: (Monad m) => Env -> Id -> ([Type],Type) -> m Env
updateFun (s,c) i sig =
	  case Map.member i s of {
	       True  -> fail ("Function redefinition:" ++ show i);
	       False -> return (Map.insert i sig s,c)
	  }

updateFuns :: (Monad m) => [(Id , ([Type],Type))] -> m Env
updateFuns = foldl (\ r (i, args) -> r >>= \ e -> updateFun e i args) (return emptyEnv)

newBlock  :: Env -> Env
newBlock (s,cs) = (s,Map.empty:cs)

emptyEnv  :: Env
emptyEnv = (Map.empty,[Map.empty])
