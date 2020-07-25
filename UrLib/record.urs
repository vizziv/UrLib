(* Project onto a given field. *)
val proj :
    nm :: Name -> t ::: Type -> drop ::: {Type} -> [[nm] ~ drop] =>
    $([nm = t] ++ drop)
    -> t

(* Extract the value from a one-field record. *)
val proj1 : nm ::: Name -> t ::: Type -> {nm : t} -> t

(* Project onto an inferred subset of fields. *)
val projs :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    $(keep ++ drop)
    -> $keep

(* Create a one-field record. *)
val inj : nm :: Name -> t ::: Type -> t -> {nm : t}

(* Rename a field. *)
val rename :
    nm1 :: Name -> nm2 :: Name -> t ::: Type -> ts ::: {Type} ->
    [[nm1] ~ ts] => [[nm2] ~ ts] =>
    $([nm1 = t] ++ ts)
    -> $([nm2 = t] ++ ts)

(* Split a record function argument into two arguments. *)
val curry :
    have ::: {Type} -> need ::: {Type} -> t ::: Type -> [have ~ need] =>
    ($(have ++ need) -> t)
    -> $have -> $need -> t

(* Append a field-value pair to a record. *)
val snoc :
    ts ::: {Type} -> $ts -> nm :: Name -> t ::: Type -> [[nm] ~ ts] =>
    t
    -> $([nm = t] ++ ts)

(* Concatenate two records. *)
val concat :
    ts1 ::: {Type} -> ts2 ::: {Type} -> [ts1 ~ ts2] =>
    $ts1 -> $ts2
    -> $(ts1 ++ ts2)

(* A swiss-army knife record modifier. *)
val set :
    keep ::: {Type} -> drop ::: {Type} -> insert ::: {Type} ->
    [keep ~ drop] => [keep ~ insert] =>
    $(keep ++ drop) -> $(insert)
    -> $(keep ++ insert)

(* Create a record where unspecified fields contain [None]. *)
val injqs :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    folder keep -> folder drop ->
    $keep
    -> $(map option (keep ++ drop))

(* Create a [show] instance for a record type. *)
val mkShow :
    ts ::: {Type} -> folder ts -> $(map show ts) ->
    $(map (fn _ => string) ts)
    -> show $ts
