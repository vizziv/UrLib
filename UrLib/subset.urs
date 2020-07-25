(* Subset type: [t fields keep] means [keep] is a subset of [fields]. *)
class t :: {Type} -> {Type} -> Type

(* If [fields] is [keep ++ drop], then [keep] is a subset of [fields]. *)
val intro :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    folder keep -> folder drop
    -> t (keep ++ drop) keep

(* Subsets are preserved by functions. *)
val mp :
    fields ::: {Type} -> keep ::: {Type} -> f ::: (Type -> Type) ->
    t fields keep
    -> t (map f fields) (map f keep)

(* General eliminator for using a subset. *)
val elim :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep ->
    res ::: Type ->
    (drop ::: {Type} -> [keep ~ drop] =>
     folder keep -> folder drop -> Eq.t fields (keep ++ drop)
     -> res)
    -> res

(* Folder for fields in the subset. *)
val fl :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep -> folder keep

(* Project onto fields in the subset. *)
val projs :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep -> $fields -> $keep

(* Set fields in the subset. *)
val set :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep ->
    $fields -> $keep
    -> $fields

(* Apply a function to fields in the subset. *)
val over :
    fields ::: {Type} -> keep ::: {Type} -> t fields keep ->
    ($keep -> $keep)
    -> $fields -> $fields

(* Create a record where fields outside the subset contain [None]. *)
val injqs :
    fields ::: {Type} -> keep ::: {Type} ->
    t fields keep ->
    $keep
    -> $(map option fields)
