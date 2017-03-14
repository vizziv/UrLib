signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    type void = variant []
end

include Types

val id : t ::: Type -> t -> t

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

(* For promising, without proof, that a branch cannot be taken. *)
val impossible : t ::: Type -> string -> t

structure Functor : sig
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
end

val monad_ident : monad (fn t => t)

val bit : bool -> int

val maximum : t ::: Type -> ord t -> t -> list t -> t
val minimum : t ::: Type -> ord t -> t -> list t -> t

val mapiPartial :
    a ::: Type -> b ::: Type -> (int -> a -> option b) -> list a -> list b

val distinct : t ::: Type -> eq t -> ord t -> list t -> bool

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
    tf :: ({K} -> K -> Type) ->
    r ::: {K} -> folder r ->
    (others :: {K} -> nm :: Name -> t ::: K -> [[nm] ~ others] =>
     folder others -> Eq.t ([nm = t] ++ others) r
     -> tf ([nm = t] ++ others) t)
    -> $(map (tf r) r)

val mapNm :
    K -->
    tf1 :: (K -> Type) -> tf2 :: ({K} -> K -> Type) ->
    r ::: {K} -> folder r ->
    (others :: {K} -> nm :: Name -> t ::: K -> [[nm] ~ others] =>
     folder others -> Eq.t ([nm = t] ++ others) r ->
     tf1 t
     -> tf2 ([nm = t] ++ others) t) ->
    $(map tf1 r)
    -> $(map (tf2 r) r)

val cases :
    ts ::: {Type} -> u ::: Type ->
    $(map (fn t => t -> u) ts) ->
    variant ts
    -> u

val casesGet :
    K -->
    r ::: {K} -> folder r -> t ::: Type ->
    variant (map (fn _ => t) r)
    -> t

val casesFunctor :
    r ::: {Type} -> folder r ->
    f ::: (Type -> Type) -> Functor.t f ->
    variant (map f r)
    -> f (variant r)

val casesMap :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    $(map (fn t :: K => tf1 t -> tf2 t) r) ->
    variant (map tf1 r)
    -> variant (map tf2 r)

val casesMapU :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (t ::: K -> tf1 t -> tf2 t) ->
    variant (map tf1 r)
    -> variant (map tf2 r)

val casesTraverse :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    f ::: (Type -> Type) -> Functor.t f ->
    $(map (fn t :: K => tf1 t -> f (tf2 t)) r) ->
    variant (map tf1 r)
    -> f (variant (map tf2 r))

val casesDiag :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> option (variant (map tf3 r))

val casesDiagU :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (t ::: K -> tf1 t -> tf2 t -> tf3 t) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> option (variant (map tf3 r))

val casesDiagTraverse :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    f ::: (Type -> Type) -> monad f ->
    $(map (fn t :: K => tf1 t -> tf2 t -> f (tf3 t)) r) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> f (option (variant (map tf3 r)))
