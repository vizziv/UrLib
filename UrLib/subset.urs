class t :: {Type} -> {Type} -> Type

val intro :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    folder keep -> folder drop
    -> t (keep ++ drop) keep

val mp :
    fields ::: {Type} -> keep ::: {Type} -> f ::: (Type -> Type) ->
    t fields keep
    -> t (map f fields) (map f keep)

val elim :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep ->
    res ::: Type ->
    (drop ::: {Type} -> [keep ~ drop] =>
     folder keep -> folder drop -> Eq.t fields (keep ++ drop)
     -> res)
    -> res

val fl :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep -> folder keep

val projs :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep -> $fields -> $keep

val injqs :
    fields ::: {Type} -> keep ::: {Type} ->
    t fields keep ->
    $keep
    -> $(map option fields)
