include Prelude.Types

datatype insertResult = Inserted | NotInserted

val eq_insertResult : eq insertResult

val sqlInjectRow :
    tables ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} ->
    fields ::: {Type} ->
    folder fields -> $(map sql_injectable fields) ->
    $fields
    -> $(map (sql_exp tables agg exps) fields)

val insert :
    fields ::: {Type} -> uniques ::: {{Unit}} ->
    folder fields -> $(map sql_injectable fields) ->
    sql_table fields uniques ->
    $fields
    -> tunit

val update :
    unchanged ::: {Type} -> uniques ::: {{Unit}} ->
    changed ::: {Type} -> [changed ~ unchanged] =>
    folder changed -> $(map sql_injectable changed) ->
    sql_table (changed ++ unchanged) uniques ->
    sql_exp [T = changed ++ unchanged] [] [] bool ->
    $changed
    -> tunit

val delete :
    fields ::: {Type} -> uniques ::: {{Unit}} ->
    sql_table fields uniques ->
    sql_exp [T = fields] [] [] bool
    -> tunit

val select :
    vals ::: {Type} -> others ::: {Type} -> [vals ~ others] =>
    tabl ::: Type -> fieldsOf tabl (vals ++ others) ->
    tabl ->
    sql_exp [T = vals ++ others] [] [] bool
    -> sql_query [] [] [T = vals] []

val count :
    fields ::: {Type} -> tabl ::: Type ->
    fieldsOf tabl fields ->
    tabl ->
    sql_exp [T = fields] [] [] bool
    -> transaction int

val exists :
    fields ::: {Type} -> tabl ::: Type ->
    fieldsOf tabl fields ->
    tabl ->
    sql_exp [T = fields] [] [] bool
    -> transaction bool

val lookup :
    tabs ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} ->
    tab ::: Name -> keys ::: {Type} -> others ::: {Type} ->
    [keys ~ others] => [[tab] ~ tabs] =>
    folder keys -> $(map sql_injectable keys) ->
    $keys
    -> sql_exp ([tab = keys ++ others] ++ tabs) agg exps bool

val lookups :
    tabs ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} ->
    tab ::: Name -> keys ::: {Type} -> others ::: {Type} ->
    [keys ~ others] => [[tab] ~ tabs] =>
    folder keys -> $(map sql_injectable keys) ->
    list $keys
    -> sql_exp ([tab = keys ++ others] ++ tabs) agg exps bool

val selectLookup :
    keys ::: {Type} -> vals ::: {Type} -> others ::: {Type} ->
    [keys ~ vals] => [keys ~ others] => [vals ~ others] =>
    folder keys -> $(map sql_injectable keys) ->
    tabl ::: Type -> fieldsOf tabl (keys ++ vals ++ others) ->
    tabl ->
    $keys
    -> sql_query [] [] [T = vals] []

val countLookup :
    keys ::: {Type} -> others ::: {Type} ->
    [keys ~ others] =>
    folder keys -> $(map sql_injectable keys) ->
    tabl ::: Type -> fieldsOf tabl (keys ++ others) ->
    tabl ->
    $keys
    -> transaction int

val existsLookup :
    keys ::: {Type} -> others ::: {Type} ->
    [keys ~ others] =>
    folder keys -> $(map sql_injectable keys) ->
    tabl ::: Type -> fieldsOf tabl (keys ++ others) ->
    tabl ->
    $keys
    -> transaction bool

(* Inserts the value if the key does not exist yet. *)
(* Result indicates succful or failed insertion. *)
(* Only works on Postgres! *)
val insertLookup :
    keys ::: {Type} -> vals ::: {Type} ->
    nm ::: Name -> uniques ::: {{Unit}} ->
    [keys ~ vals] => [[nm] ~ uniques] =>
    folder keys -> $(map sql_injectable keys) ->
    folder vals -> $(map sql_injectable vals) ->
    sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques) ->
    $keys ->
    $vals
    -> transaction insertResult

val updateLookup :
    keys ::: {Type} -> vals ::: {Type} -> others ::: {Type} ->
    uniques ::: {{Unit}} ->
    [keys ~ vals] => [keys ~ others] => [vals ~ others] =>
    folder keys -> $(map sql_injectable keys) ->
    folder vals -> $(map sql_injectable vals) ->
    sql_table (keys ++ vals ++ others) uniques ->
    $keys ->
    $vals
    -> tunit

(* Inserts or updates the value depeding on whether the key already exists. *)
(* Result indicates whether row was inserted or updated. *)
(* Only works on Postgres! *)
val setLookup :
    keys ::: {Type} -> vals ::: {Type} ->
    nm ::: Name -> uniques ::: {{Unit}} ->
    [keys ~ vals] => [[nm] ~ uniques] =>
    folder keys -> $(map sql_injectable keys) ->
    folder vals -> $(map sql_injectable vals) ->
    sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques) ->
    $keys ->
    $vals
    -> transaction insertResult

val deleteLookup :
    keys ::: {Type} -> others ::: {Type} -> uniques ::: {{Unit}} ->
    [keys ~ others] =>
    folder keys -> $(map sql_injectable keys) ->
    sql_table (keys ++ others) uniques ->
    $keys
    -> tunit

(* Only works on Postgres! *)
val insertRandKeys :
    keys ::: {Type} -> vals ::: {Type} ->
    nm ::: Name -> uniques ::: {{Unit}} ->
    [keys ~ vals] => [[nm] ~ uniques] =>
    folder keys -> $(map Random.t keys) -> $(map sql_injectable keys) ->
    folder vals -> $(map sql_injectable vals) ->
    sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques) ->
    $vals
    -> transaction $keys

(* Only works on Postgres! *)
val updateRandKeys :
    keys ::: {Type} -> vals ::: {Type} ->
    nm ::: Name -> uniques ::: {{Unit}} ->
    [keys ~ vals] => [[nm] ~ uniques] =>
    folder keys -> $(map Random.t keys) -> $(map sql_injectable keys) ->
    sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques) ->
    $keys
    -> transaction $keys

val compat :
    tabs ::: {{Type}} -> agg ::: {{Type}} -> exps ::: {Type} ->
    tab ::: Name -> keys ::: {Type} -> others ::: {Type} ->
    [keys ~ others] => [[tab] ~ tabs] =>
    folder keys -> $(map sql_injectable_prim keys) ->
    $(map option keys)
    -> sql_exp ([tab = (map option keys) ++ others] ++ tabs) agg exps bool

val selectCompat :
    keys ::: {Type} -> vals ::: {Type} -> others ::: {Type} ->
    [keys ~ vals] => [keys ~ others] => [vals ~ others] =>
    folder keys -> $(map sql_injectable_prim keys) ->
    tabl ::: Type -> fieldsOf tabl (map option keys ++ vals ++ others) ->
    tabl ->
    $(map option keys)
    -> sql_query [] [] [T = vals] []
