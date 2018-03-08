include Prelude.Types

class t :: (Type -> Type) -> Type

val mk :
    f ::: (Type -> Type) ->
    (a ::: Type -> b ::: Type -> (a -> b) -> f a -> f b)
    -> t f

val mp :
    f ::: (Type -> Type) -> t f ->
    a ::: Type -> b ::: Type ->
    (a -> b)
    -> f a -> f b

val monad : f ::: (Type -> Type) -> monad f -> t f

val list : t list

val field :
    nm :: Name -> ts ::: {Type} -> [[nm] ~ ts]
    => t (fn t => $([nm = t] ++ ts))

val choice :
    nm :: Name -> ts ::: {Type} -> [[nm] ~ ts] => folder ts
    -> t (fn t => variant ([nm = t] ++ ts))

val compose :
    f ::: (Type -> Type) -> g ::: (Type -> Type) ->
    t f -> t g
    -> t (compose f g)
