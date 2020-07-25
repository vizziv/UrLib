include Prelude.Types

(* Functor class (but called "mappable" to avoid clash with keyword). *)
class t :: (Type -> Type) -> Type

(* If you can implement [mp], then you're mappable. *)
val mk :
    f ::: (Type -> Type) ->
    (a ::: Type -> b ::: Type -> (a -> b) -> f a -> f b)
    -> t f

(* The primary method of the class. *)
val mp :
    f ::: (Type -> Type) -> t f ->
    a ::: Type -> b ::: Type ->
    (a -> b)
    -> f a -> f b

(* Any [monad] is a functor. *)
val monad : f ::: (Type -> Type) -> monad f -> t f

(* [list] is a functor. *)
val list : t list

(* Any record type with a one-field "hole" is a functor. *)
val field :
    nm :: Name -> ts ::: {Type} -> [[nm] ~ ts]
    => t (fn t => $([nm = t] ++ ts))

(* Any variant type with a one-field "hole" is a functor. *)
val choice :
    nm :: Name -> ts ::: {Type} -> [[nm] ~ ts] => folder ts
    -> t (fn t => variant ([nm = t] ++ ts))

(* If [f] and [g] are functors, so is [compose f g]. *)
val compose :
    f ::: (Type -> Type) -> g ::: (Type -> Type) ->
    t f -> t g
    -> t (compose f g)
