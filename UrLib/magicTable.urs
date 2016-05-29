include Prelude.Types

con filter :: {Type} -> Type
val lookup : keys ::: {Type} -> others ::: {Type} -> [keys ~ others]
             => folder keys -> folder others -> $(map sql_injectable keys)
             -> $keys -> filter (keys ++ others)

con query :: {Type} -> {Type} -> Type
val select : keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop]
             => folder keep
             -> filter (keep ++ drop) -> query (keep ++ drop) keep

signature Types = sig
    con fields :: {Type}
end

signature Input = sig
    include Types
    con chan :: Name
    constraint [chan] ~ fields
    val fl : folder fields
    val eq_fields : $(map eq fields)
    val sql_fields : $(map sql_injectable_prim fields)
    (* For debugging. *)
    val show_fields : $(map show fields)
    val labels_fields : $(map (fn _ => string) fields)
end

signature Output = sig
    include Types
    val insert : $fields -> tunit
    val update : write ::: {Type} -> Subset.t fields write
                 -> $write -> filter fields -> tunit
    val delete : filter fields -> tunit
    con connection :: {Type} -> Type
    val connect : read ::: {Type}
                  -> query fields read -> transaction (connection read)
    val listen : read ::: {Type} -> Subset.t fields read
                 -> connection read -> tunit
    val value : read ::: {Type}
                -> connection read -> LinkedList.signals $read
end

functor Make(M : Input) : Output
    where con fields = M.fields
