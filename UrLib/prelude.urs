signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    con const = K1 ==> K2 ==> fn (t1 :: K1) => fn (t2 :: K2) => t1
    type void = variant []
end

include Types

val ident : t ::: Type -> t -> t

val const : a ::: Type -> b ::: Type -> a -> b -> a

val on :
    a ::: Type -> b ::: Type -> c ::: Type ->
    (b -> b -> c) -> (a -> b)
    -> a -> a -> c

val zip :
    a ::: Type -> b ::: Type -> c ::: Type ->
    (a -> b -> c)
    -> list a -> list b -> list c

(* For proving that a branch cannot be taken. *)
val contradiction : t ::: Type -> void -> t

(* For promising without proof that a branch cannot be taken. *)
val impossible : t ::: Type -> string -> t

val monad_ident : monad (fn t => t)

val bit : bool -> int

val maximum : t ::: Type -> ord t -> t -> list t -> t
val minimum : t ::: Type -> ord t -> t -> list t -> t

val mapiPartial :
    a ::: Type -> b ::: Type -> (int -> a -> option b) -> list a -> list b

val distinct : t ::: Type -> eq t -> ord t -> list t -> bool

(* Does the action only when the given condition holds. *)
val when : m ::: (Type -> Type) -> monad m -> bool -> m unit -> m unit

val spawnListener : t ::: Type -> (t -> tunit) -> channel t -> tunit

val xempty : ctx ::: {Unit} -> xml ctx [] []

val xdyn :
    ctx ::: {Unit} -> [[Dyn] ~ ctx] =>
    signal (xml ([Dyn] ++ ctx) [] [])
    -> (xml ([Dyn] ++ ctx) [] [])

val xactive : transaction xbody -> xbody

val xaction : tunit -> xbody

val mapNm0 :
    K -->
    tf :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (others :: {K} -> nm :: Name -> t ::: K -> [[nm] ~ others] =>
     folder others -> Eq.t ([nm = t] ++ others) r
     -> tf t)
    -> $(map tf r)

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

val cases :
    ts ::: {Type} -> u ::: Type ->
    $(map (fn t => t -> u) ts) ->
    variant ts
    -> u
