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

data PolyType = ForAll [Identifier] MonoType
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

substitute :: Sub -> MonoType -> MonoType
substitute sub (Arrow m1 m2) = Arrow (substitute sub m1) (substitute sub m2)
substitute sub (TypeVar s) = case Map.lookup s (runSub sub) of
                               Just m -> m
                               Nothing -> TypeVar s

---------------
-- Inference --
---------------

newtype TypeEnv = TypeEnv { runTypeEnv :: Map.Map Identifier PolyType }

type Constraint = (MonoType, MonoType)

type Infer = RWS.RWST TypeEnv [Constraint] Identifier Maybe

freshVar :: Infer MonoType
freshVar = do
  oldState <- RWS.get
  let freshId = runIdentifier oldState
  RWS.put $ Identifier $ freshId + 1
  return $ TypeVar $ Identifier freshId

lookupEnv :: Identifier -> Infer MonoType
lookupEnv x = do
  env <- RWS.ask
  polytype <- Trans.lift $ Map.lookup x $ runTypeEnv env
  instantiate polytype

inExtendedEnv :: Identifier -> PolyType -> Infer a -> Infer a
inExtendedEnv x polytype action = RWS.local (\env ->
    TypeEnv $ Map.insert x polytype $ runTypeEnv env
  ) action

generalize :: MonoType -> TypeEnv -> PolyType
generalize monotype env = ForAll (
    (freeVars monotype) List.\\ (Map.keys $ runTypeEnv env)
  ) monotype

instantiate :: PolyType -> Infer MonoType
instantiate (ForAll vars t) =
  do freshVars <- mapM (const freshVar) vars
     return $ substitute (Sub $ Map.fromList $ zip vars freshVars) t

infer :: Term -> Infer MonoType
infer (Variable x) = lookupEnv x
infer (Abstraction x t) = do
  argType <- freshVar
  retType <- inExtendedEnv x (ForAll [] argType) (infer t)
  return $ Arrow argType retType
infer (Application t1 t2) = do
  absType <- infer t1
  argType <- infer t2
  retType <- freshVar
  RWS.tell [(absType, Arrow argType retType)]
  return retType
infer (Let x t1 t2) = do
  env <- RWS.ask
  defType <- infer t1
  bodyType <- inExtendedEnv x (generalize defType env) (infer t2)
  return bodyType
infer (Provide t1 t2) = undefined
infer Implicit = undefined

startIdentifier :: Term -> Identifier
startIdentifier (Variable x) = Identifier $
  1 + runIdentifier x
startIdentifier (Abstraction x t) = Identifier $
  1 + max (runIdentifier x) (runIdentifier $ startIdentifier t)
startIdentifier (Application t1 t2) = Identifier $
  1 + max (runIdentifier $ startIdentifier t1) (runIdentifier $ startIdentifier t2)
startIdentifier (Let x t1 t2) = Identifier $
  1 + max (runIdentifier x) (max (runIdentifier $ startIdentifier t1) (runIdentifier $ startIdentifier t2))
startIdentifier (Provide t1 t2) = Identifier $
  1 + max (runIdentifier $ startIdentifier t1) (runIdentifier $ startIdentifier t2)
startIdentifier Implicit = Identifier 0

runInfer :: Term -> Maybe MonoType
runInfer t = fst <$> RWS.evalRWST (infer t) (TypeEnv Map.empty) (startIdentifier t)
