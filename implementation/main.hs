data Term = Variable String
          | Abstraction String Term
          | Application Term Term
          | Let String Term Term
          | Provide Term Term
          | Implicit

data MonoType = Arrow MonoType MonoType
              | TypeVar String

data PolyType = Mono MonoType
              | Forall [String] [PolyType] MonoType
