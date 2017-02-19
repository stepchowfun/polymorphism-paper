module Lib where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Trans.RWS.Lazy as RWS
import qualified Control.Monad.Trans.State as StateT
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

------------
-- Syntax --
------------

newtype Identifier = Identifier { runIdentifier :: Int }
                   deriving (Eq, Ord, Show)

data Term = Variable Identifier
          | Abstraction Identifier Term
          | Application Term Term
          | Let Identifier Term Term
          | Provide Term Term
          | Implicit
          deriving Show

data MonoType = Arrow MonoType MonoType
              | TypeVar Identifier
              deriving (Eq, Show)

data PolyType = Mono MonoType
              | Forall [Identifier] MonoType
              deriving Show

-------------------
-- Substitutions --
-------------------

newtype Sub = Sub { runSub :: Map.Map Identifier MonoType }

freeVars :: MonoType -> [Identifier]
freeVars (Arrow t1 t2) = freeVars t1 ++ freeVars t2
freeVars (TypeVar s) = [s]

occurs :: Identifier -> MonoType -> Bool
occurs x expr = elem x $ freeVars expr

mgu :: Sub -> Sub -> Maybe Sub
mgu sub1 sub2 = if Map.intersection (runSub sub1) (runSub sub2) ==
                   Map.intersection (runSub sub2) (runSub sub1)
                  then Just $ Sub $ Map.union (runSub sub1) (runSub sub2)
                  else Nothing

unify :: MonoType -> MonoType -> Maybe Sub
unify (TypeVar x) t = if occurs x t
                        then Nothing
                        else Just $ Sub $ Map.singleton x t
unify t (TypeVar x) = unify (TypeVar x) t
unify (Arrow m1 m2) (Arrow n1 n2) =
  do sub1 <- unify m1 n1
     sub2 <- unify m1 n1
     mgu sub1 sub2

substitute :: Sub -> MonoType -> MonoType
substitute sub (Arrow m1 m2) = Arrow (substitute sub m1) (substitute sub m2)
substitute sub (TypeVar s) = case Map.lookup s (runSub sub) of
                               Just m -> m
                               Nothing -> TypeVar s

--------------------------------------
-- Generalization and instantiation --
--------------------------------------

type FreshMonoType = StateT.State Identifier MonoType

newVar :: FreshMonoType
newVar = StateT.state $ \x -> (TypeVar x, Identifier $ (runIdentifier x) + 1)

generalize :: MonoType -> PolyType
generalize t = Forall (freeVars t) t

instantiate :: PolyType -> FreshMonoType
instantiate (Mono t) = return t
instantiate (Forall vars t) =
  do freshVars <- mapM (const newVar) vars
     return $ substitute (Sub $ Map.fromList $ zip vars freshVars) t

-----------------------
-- Type environments --
-----------------------

newtype TypeEnv = TypeEnv { runTypeEnv :: Map.Map Identifier PolyType }

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty

addType :: Identifier -> PolyType -> TypeEnv -> TypeEnv
addType x t e = TypeEnv $ Map.insert x t $ runTypeEnv e

lookupType :: Identifier -> TypeEnv -> Maybe PolyType
lookupType x e = Map.lookup x $ runTypeEnv e

---------------
-- Inference --
---------------

type Infer = RWS.RWST TypeEnv Sub Identifier Maybe MonoType
