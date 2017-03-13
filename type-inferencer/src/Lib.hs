module Lib where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Trans.RWS.Lazy as RWS
import qualified Control.Monad.Trans.State as StateT
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

------------
-- Syntax --
------------

newtype Identifier = Identifier { runIdentifier :: Int }
                   deriving (Eq, Ord)

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

-----------------------
-- Utility functions --
-----------------------

instance Show Identifier where
  show (Identifier x) = "Identifier " ++ show x

------------------
-- Substitution --
------------------

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

unify :: MonoType -> MonoType -> Maybe Sub
unify (TypeVar x) t = if occurs x t
                        then Nothing
                        else Just $ Sub $ Map.singleton x t
unify t (TypeVar x) = unify (TypeVar x) t
unify (Arrow m1 m2) (Arrow n1 n2) =
  do sub1 <- unify m1 n1
     sub2 <- unify m1 n1
     Just $ compose sub1 sub2

-- Law: substitute (compose s1 s2) t = substitute s1 (substitute s2 t)
compose :: Sub -> Sub -> Sub
compose s1 s2 = Sub $ Map.filterWithKey (\k -> \v -> TypeVar k /= v) $
  Map.union
    (Map.map (substitute s1) (runSub s2))
    (Map.filterWithKey
      (\k -> \v -> Maybe.isNothing $ Map.lookup k (runSub s2))
      (runSub s1)
    )

instance Monoid Sub where
  mempty = Sub Map.empty
  mappend = compose

---------------
-- Inference --
---------------

newtype TypeEnv = TypeEnv { runTypeEnv :: Map.Map Identifier PolyType }

newtype ImplicitEnv = ImplicitEnv { runImplicitEnv :: [MonoType] }

type Infer = RWS.RWST (TypeEnv, ImplicitEnv) Sub Identifier Maybe

freshVar :: Infer MonoType
freshVar = do
  oldState <- RWS.get
  let freshId = runIdentifier oldState
  RWS.put $ Identifier $ freshId + 1
  return $ TypeVar $ Identifier freshId

lookupEnv :: Identifier -> Infer MonoType
lookupEnv x = do
  (env, _) <- RWS.ask
  polytype <- Trans.lift $ Map.lookup x $ runTypeEnv env
  instantiate polytype

inExtendedTypeEnv :: Identifier -> PolyType -> Infer a -> Infer a
inExtendedTypeEnv x polytype action = RWS.local (\(typeEnv, implicitEnv) ->
    (TypeEnv $ Map.insert x polytype $ runTypeEnv typeEnv, implicitEnv)
  ) action

inExtendedImplicitEnv :: MonoType -> Infer a -> Infer a
inExtendedImplicitEnv monotype action = RWS.local (\(typeEnv, implicitEnv) ->
    (typeEnv, ImplicitEnv $ monotype : (runImplicitEnv implicitEnv))
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
  retType <- inExtendedTypeEnv x (ForAll [] argType) (infer t)
  return $ Arrow argType retType
infer (Application t1 t2) = do
  absType <- infer t1
  argType <- infer t2
  retType <- freshVar
  RWS.tell $ Maybe.fromJust $ unify absType (Arrow argType retType)
  return retType
infer (Let x t1 t2) = do
  (env, _) <- RWS.ask
  defType <- infer t1
  bodyType <- inExtendedTypeEnv x (generalize defType env) (infer t2)
  return bodyType
infer (Provide t1 t2) = do
  providedType <- infer t1
  bodyType <- inExtendedImplicitEnv providedType (infer t2)
  return bodyType
infer Implicit = undefined

startIdentifier :: Term -> Identifier
startIdentifier (Variable x) = Identifier $
  1 + runIdentifier x
startIdentifier (Abstraction x t) = Identifier $
  1 + max (runIdentifier x) (runIdentifier $ startIdentifier t)
startIdentifier (Application t1 t2) = Identifier $
  1 + max
    (runIdentifier $ startIdentifier t1)
    (runIdentifier $ startIdentifier t2)
startIdentifier (Let x t1 t2) = Identifier $
  1 + max
  (runIdentifier x)
  (max
    (runIdentifier $ startIdentifier t1)
    (runIdentifier $ startIdentifier t2)
  )
startIdentifier (Provide t1 t2) = Identifier $
  1 + max
    (runIdentifier $ startIdentifier t1)
    (runIdentifier $ startIdentifier t2)
startIdentifier Implicit = Identifier 0

runInfer :: Term -> Maybe (MonoType, Sub)
runInfer t = RWS.evalRWST
  (infer t)
  (TypeEnv Map.empty, ImplicitEnv [])
  (startIdentifier t)
