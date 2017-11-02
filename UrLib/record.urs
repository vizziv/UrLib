val proj :
    nm :: Name -> t ::: Type -> drop ::: {Type} -> [[nm] ~ drop] =>
    $([nm = t] ++ drop)
    -> t

val proj1 : nm ::: Name -> t ::: Type -> {nm : t} -> t

val projs :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    $(keep ++ drop)
    -> $keep

val inj : nm :: Name -> t ::: Type -> t -> {nm : t}

val rename :
    nm1 :: Name -> nm2 :: Name -> t ::: Type -> ts ::: {Type} ->
    [[nm1] ~ ts] => [[nm2] ~ ts] =>
    $([nm1 = t] ++ ts)
    -> $([nm2 = t] ++ ts)

val curry :
    have ::: {Type} -> need ::: {Type} -> t ::: Type -> [have ~ need] =>
    ($(have ++ need) -> t)
    -> $have -> $need -> t

val snoc :
    ts ::: {Type} -> $ts -> nm :: Name -> t ::: Type -> [[nm] ~ ts] =>
    t
    -> $([nm = t] ++ ts)

val set :
    keep ::: {Type} -> drop ::: {Type} -> insert ::: {Type} ->
    [keep ~ drop] => [keep ~ insert] =>
    $(keep ++ drop) -> $(insert)
    -> $(keep ++ insert)

val injqs :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    folder keep -> folder drop ->
    $keep
    -> $(map option (keep ++ drop))

val mkShow :
    ts ::: {Type} -> folder ts -> $(map show ts) ->
    $(map (fn _ => string) ts)
    -> show $ts
