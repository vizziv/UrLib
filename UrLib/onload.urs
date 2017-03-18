include Prelude.Types

con t :: Type -> Type

val monad : monad t

val server : a ::: Type -> transaction a -> t a

val client : tunit -> t unit

val run : t xbody -> transaction page
