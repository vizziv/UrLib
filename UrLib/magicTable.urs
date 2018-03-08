include Prelude.Types

con t :: {Type} -> Type
con filter :: {Type} -> Type
con query :: {Type} -> {Type} -> Type
con connection :: {Type} -> {Type} -> Type

val lookup :
    keys ::: {Type} -> others ::: {Type} -> [keys ~ others] =>
    folder keys -> folder others -> $(map sql_injectable keys) ->
    $keys
    -> filter (keys ++ others)

val select :
    keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop] =>
    folder keep ->
    filter (keep ++ drop)
    -> query (keep ++ drop) keep

val insert : fields ::: {Type} -> t fields -> $fields -> tunit

val update :
    write ::: {Type} -> others ::: {Type} -> [write ~ others] =>
    folder write -> folder others ->
    t (write ++ others) -> $write -> filter (write ++ others)
    -> tunit

val delete : fields ::: {Type} -> t fields -> filter fields -> tunit

val connect :
    read ::: {Type} -> others ::: {Type} -> [read ~ others] =>
    folder read -> folder others ->
    t (read ++ others) -> query (read ++ others) read
    -> transaction (connection (read ++ others) read)

val listen :
    fields ::: {Type} -> read ::: {Type} ->
    connection fields read
    -> tunit

val value :
    read ::: {Type} -> others ::: {Type} -> [read ~ others] =>
    connection (read ++ others) read
    -> LinkedList.signals $read

signature Input = sig
    con fields :: {Type}
    con chan :: Name
    constraint [chan] ~ fields
    val fl_fields : folder fields
    val eq_fields : $(map eq fields)
    val sqlp_fields : $(map sql_injectable_prim fields)
    (* For debugging. *)
    val show_fields : $(map show fields)
    val label_fields : $(map (fn _ => string) fields)
end

functor Make(M : Input) : sig
    val t : t M.fields
    (* For debugging. *)
    table tab : M.fields
end
