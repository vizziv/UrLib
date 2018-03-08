class t :: Type -> Type

val mk : a ::: Type -> (int -> a) -> t a

val gen : a ::: Type -> t a -> transaction a

val int : t int
