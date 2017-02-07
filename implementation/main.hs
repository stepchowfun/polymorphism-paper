import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Unique

data Term = Variable String
          | Abstraction String Term
          | Application Term Term
          | Let String Term Term
          | Provide Term Term
          | Implicit

data MonoType = Arrow MonoType MonoType
              | TypeVar String
              deriving Eq

data PolyType = Mono MonoType
              | Forall [String] MonoType

newtype TypeEnv = TypeEnv (Map.Map String PolyType)

type Infer a = MaybeT (State Unique) a
type Subst = Map.Map String MonoType

occurs :: String -> MonoType -> Bool
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

main = putStr "Hello Stephan"
