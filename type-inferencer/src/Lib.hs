module Lib where

import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Control.Monad.Trans.State as StateT
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

type Identifier = Int

data Term = Variable Identifier
          | Abstraction Identifier Term
          | Application Term Term
          | Let Identifier Term Term
          | Provide Term Term
          | Implicit

data MonoType = Arrow MonoType MonoType
              | TypeVar Identifier
              deriving Eq

data PolyType = Mono MonoType
              | Forall [Identifier] MonoType

type TypeEnv = Map.Map Identifier PolyType
type Infer a = MaybeT.MaybeT (StateT.State Identifier) a
type Subst = Map.Map Identifier MonoType

monoFree :: MonoType -> [Identifier]
monoFree (Arrow t1 t2) = monoFree t1 ++ monoFree t2
monoFree (TypeVar s) = [s]

polyFree :: PolyType -> [Identifier]
polyFree (Mono t) = monoFree t
polyFree (Forall vars t) = monoFree t List.\\ vars

occurs :: Identifier -> MonoType -> Bool
occurs x1 (TypeVar x2) = x1 == x2
occurs x (Arrow t1 t2) = occurs x t1 || occurs x t2

mgu :: Subst -> Subst -> Maybe Subst
mgu sub1 sub2 = if Map.intersection sub1 sub2 == Map.intersection sub2 sub1
                then Just (Map.union sub1 sub2)
                else Nothing

substitute :: Subst -> MonoType -> MonoType
substitute sub (Arrow m1 m2) = Arrow (substitute sub m1) (substitute sub m2)
substitute sub (TypeVar s) = case Map.lookup s sub of
                             Just m -> m
                             Nothing -> TypeVar s

unify :: MonoType -> MonoType -> Maybe Subst
unify (TypeVar x) t = if occurs x t then Nothing else Just (Map.singleton x t)
unify t (TypeVar x) = unify (TypeVar x) t
unify (Arrow m1 m2) (Arrow n1 n2) =
  do sub1 <- unify m1 n1
     sub2 <- unify m1 n1
     mgu sub1 sub2

generalize :: MonoType -> PolyType
generalize t = Forall (monoFree t) t

instantiate :: PolyType -> StateT.State Identifier MonoType
instantiate (Mono t) = return t
instantiate (Forall vars t) =
  do freshVars <- mapM (const newVar) vars
     return $ substitute (Map.fromList $ zip vars freshVars) t

addType :: Identifier -> PolyType -> TypeEnv -> TypeEnv
addType = Map.insert

lookupType :: Identifier -> TypeEnv -> Maybe PolyType
lookupType = Map.lookup

newVar :: StateT.State Identifier MonoType
newVar = StateT.state $ \x -> (TypeVar x, x + 1)

runInInfer :: StateT.State Identifier a -> Infer a
runInInfer toRun =
  do initState <- Trans.lift StateT.get
     let (val, state) = StateT.runState toRun initState
     Trans.lift $ StateT.put state
     return val

infer :: TypeEnv -> Term -> Infer MonoType
infer ctx term = case term of
  Variable x -> do
    runInInfer $ instantiate $ Maybe.fromJust $ lookupType x ctx
  Abstraction x t -> do
    v <- runInInfer newVar
    tp <- infer (addType x (Forall [] $ v) ctx) t
    return $ Arrow v tp
  Application t1 t2 -> do
    v <- runInInfer newVar
    tp1 <- infer ctx t1
    tp2 <- infer ctx t2
    return $ substitute (Maybe.fromJust $ unify tp1 $ Arrow tp2 v) v
  Let x t1 t2 -> do
    tp1 <- infer ctx t1
    infer (addType x (generalize tp1) ctx) t2
  Provide t1 t2 -> undefined
  Implicit -> undefined
