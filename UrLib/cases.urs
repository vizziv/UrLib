include Prelude.Types

val get :
    K -->
    r ::: {K} -> folder r -> t ::: Type ->
    variant (map (const t) r)
    -> t

val mappable :
    r ::: {Type} -> folder r ->
    f ::: (Type -> Type) -> Mappable.t f ->
    variant (map f r)
    -> f (variant r)

val mp :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    $(map (fn t :: K => tf1 t -> tf2 t) r) ->
    variant (map tf1 r)
    -> variant (map tf2 r)

val mapU :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (t ::: K -> tf1 t -> tf2 t) ->
    variant (map tf1 r)
    -> variant (map tf2 r)

val traverse :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    f ::: (Type -> Type) -> Mappable.t f ->
    $(map (fn t :: K => tf1 t -> f (tf2 t)) r) ->
    variant (map tf1 r)
    -> f (variant (map tf2 r))

val diag :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> option (variant (map tf3 r))

val diagU :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) ->
    tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    (t ::: K -> tf1 t -> tf2 t -> tf3 t) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> option (variant (map tf3 r))

val diagTraverse :
    K -->
    tf1 :: (K -> Type) -> tf2 :: (K -> Type) -> tf3 :: (K -> Type) ->
    r ::: {K} -> folder r ->
    f ::: (Type -> Type) -> monad f ->
    $(map (fn t :: K => tf1 t -> tf2 t -> f (tf3 t)) r) ->
    variant (map tf1 r) -> variant (map tf2 r)
    -> f (option (variant (map tf3 r)))
