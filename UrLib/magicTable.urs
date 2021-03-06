(* DEPRECATED *)
(* Warning: this idea basically can't work for general queries, so this whole
   system needs a redesign. *)

include Prelude.Types

con t :: (* fields *) {Type} -> Type

con filter :: (* fields *) {Type} -> Type
con query :: (* fields *) {Type} -> (* read *) {Type} -> Type
con connection :: (* fields *) {Type} -> (* read *) {Type} -> Type

val lookup :
    keys ::: {Type} -> others ::: {Type} -> [keys ~ others] =>
    folder keys -> folder others -> $(map sql_injectable keys) ->
    $keys
    -> filter (keys ++ others)

val select :
    read ::: {Type} -> others ::: {Type} -> [read ~ others] =>
    folder read ->
    filter (read ++ others)
    -> query (read ++ others) read

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
    val fl : folder fields
    val eq : $(map eq fields)
    val sqlp : $(map sql_injectable_prim fields)
    structure Debug : sig
        val show : $(map show fields)
        val label : $(map (fn _ => string) fields)
    end
end

functor Make(M : Input) : sig
    val t : t M.fields
    structure Debug : sig
        table tab : M.fields
    end
end
