signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    con const = K1 ==> K2 ==> fn (t1 :: K1) => fn (t2 :: K2) => t1
    type void = variant []
end

include Types

(* Identity function. *)
val ident : t ::: Type -> t -> t

(* Constant function. *)
val const : a ::: Type -> b ::: Type -> a -> b -> a

(* [op `on` f] applies [op] after applying [f] to both arguments. *)
val on :
    a ::: Type -> b ::: Type -> c ::: Type ->
    (b -> b -> c) -> (a -> b)
    -> a -> a -> c

(* Zip lists together with given function. *)
val zip :
    a ::: Type -> b ::: Type -> c ::: Type ->
    (a -> b -> c)
    -> list a -> list b -> list c

(* Proves that the current branch is unreachable. *)
val contradiction : t ::: Type -> void -> t

(* Promises without proof that the current branch is unreachable. *)
val impossible : t ::: Type -> string -> t

(* Identity monad. *)
val monad_ident : monad (fn t => t)

(* Maps False to 0 and True to 1. *)
val bit : bool -> int

(* Maximum/minimum element of a list. *)
val maximum : t ::: Type -> ord t -> t -> list t -> t
val minimum : t ::: Type -> ord t -> t -> list t -> t

(* Indexed map-filter combination. *)
val mapiPartial :
    a ::: Type -> b ::: Type -> (int -> a -> option b) -> list a -> list b

(* Whether all elements of the list are distinct. O(n log n) time. *)
val distinct : t ::: Type -> eq t -> ord t -> list t -> bool

(* Does the action only when the given condition holds. *)
val when : m ::: (Type -> Type) -> monad m -> bool -> m unit -> m unit

(*  *)
val loop :
    m ::: (Type -> Type) -> monad m -> t ::: Type -> (t -> m t) -> t -> m void

val spawnLoop : transaction void -> tunit

val spawnListener : t ::: Type -> (t -> tunit) -> channel t -> tunit

val xempty : ctx ::: {Unit} -> xml ctx [] []

(* [<dyn/>] tag as a function. *)
val xdyn :
    ctx ::: {Unit} -> [[Dyn] ~ ctx] =>
    signal (xml ([Dyn] ++ ctx) [] [])
    -> (xml ([Dyn] ++ ctx) [] [])

(* [<active/>] tag as a function. *)
val xactive : transaction xbody -> xbody

(* A [<active/>] tag with no content, used only for its side effect. *)
val xaction : tunit -> xbody

(* An indexed version of [map0]. *)
val mapNm0 :
    K -->
    tf :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (others :: {K} -> nm :: Name -> t ::: K -> [[nm] ~ others] =>
     folder others -> Eq.t ([nm = t] ++ others) r
     -> tf t)
    -> $(map tf r)

(* An indexed version of [mp]. *)
val mapNm :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (others :: {K} -> nm :: Name -> t ::: K -> [[nm] ~ others] =>
     folder others -> Eq.t ([nm = t] ++ others) r ->
     tf1 t
     -> tf2 t) ->
    $(map tf1 r)
    -> $(map tf2 r)

(* Flipped [match]. *)
val cases :
    ts ::: {Type} -> u ::: Type ->
    $(map (fn t => t -> u) ts) ->
    variant ts
    -> u
