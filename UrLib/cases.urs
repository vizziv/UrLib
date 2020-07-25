(* Variations on the theme of [Prelude.cases]. *)

include Prelude.Types

(* If all cases have the same type, extract the value of that type. *)
val get :
    K -->
    r ::: {K} -> folder r -> t ::: Type ->
    variant (map (const t) r)
    -> t

(* Go from "choice of containers" to "container of choices". *)
val mappable :
    r ::: {Type} -> folder r ->
    f ::: (Type -> Type) -> Mappable.t f ->
    variant (map f r)
    -> f (variant r)

(* For each case, return a value with the same case name. *)
val mp :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    $(map (fn t :: K => tf1 t -> tf2 t) r) ->
    variant (map tf1 r)
    -> variant (map tf2 r)

(* Uniform version of [mp]. *)
val mapU :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (t ::: K -> tf1 t -> tf2 t) ->
    variant (map tf1 r)
    -> variant (map tf2 r)

(* Composition of [mappable] and [mp]. *)
val traverse :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    f ::: (Type -> Type) -> Mappable.t f ->
    $(map (fn t :: K => tf1 t -> f (tf2 t)) r) ->
    variant (map tf1 r)
    -> f (variant (map tf2 r))

(* Apply a case-dependent if both inputs are in the same case. *)
val diag :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> option (variant (map tf3 r))

(* Apply a polymorphic function if both inputs are in the same case. *)
val diagU :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (t ::: K -> tf1 t -> tf2 t -> tf3 t) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> option (variant (map tf3 r))

(* Composition of [traverse] and [diag] *)
val diagTraverse :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    f ::: (Type -> Type) -> monad f ->
    $(map (fn t :: K => tf1 t -> tf2 t -> f (tf3 t)) r) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> f (option (variant (map tf3 r)))
