module Lib where

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

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
type Infer a = MaybeT (State Identifier) a
type Subst = Map.Map Identifier MonoType

occurs :: Identifier -> MonoType -> Bool
occurs s1 (TypeVar s2) = s1 == s2
occurs s1 (Arrow m1 m2) = occurs s1 m1 || occurs s1 m2

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
unify (TypeVar s) t = if occurs s t then Nothing else Just (Map.singleton s t)
unify t (TypeVar s) = unify (TypeVar s) t
unify (Arrow m1 m2) (Arrow n1 n2) =
  do sub1 <- unify m1 n1
     sub2 <- unify m1 n1
     mgu sub1 sub2

instantiate :: PolyType -> State Identifier MonoType
instantiate (Mono m) = return m
instantiate (Forall vars m) =
  do freshVars <- mapM (\x -> newVar) vars
     return (substitute (Map.fromList (zip vars freshVars)) m)

lookupType :: Identifier -> TypeEnv -> PolyType
lookupType = undefined -- Map.lookup

addType :: Identifier -> PolyType -> TypeEnv -> TypeEnv
addType = Map.insert

newVar :: State Identifier MonoType
newVar = state $ \x -> (TypeVar x, x + 1)

runInInfer :: State Identifier a -> Infer a
runInInfer toRun = undefined
{-
  do initState <- get
     let (inst, st) = runState toRun initState
     put st
     return inst
-}

infer :: TypeEnv -> Term -> Infer (Subst, MonoType)
infer g term = case term of
  Variable s -> do
    v <- runInInfer (instantiate $ lookupType s g)
    return (Map.empty, v)
  Abstraction s e -> do
    v <- runInInfer newVar
    (sub, tp) <- infer (addType s (Forall [] $ v) g) e
    return (sub, substitute sub (Arrow v tp))
  Application t1 t2 -> undefined
  Let s t1 t2 -> undefined
  Provide t1 t2 -> undefined
  Implicit -> undefined
