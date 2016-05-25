include Prelude.Types

con filter :: {Type} -> Type
val lookup : keys ::: {Type} -> others ::: {Type} -> [keys ~ others]
             => folder keys -> folder others -> $(map sql_injectable keys)
             -> $keys -> filter (keys ++ others)

con query :: {Type} -> Type -> Type
val select : keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop]
             => folder keep
             -> filter (keep ++ drop) -> query (keep ++ drop) $keep

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
end

signature Output = sig
    include Types
    val insert : $fields -> tunit
    val update : $(map option fields) -> filter fields -> tunit
    val delete : filter fields -> tunit
    con connection :: Type -> Type
    val connect : a ::: Type -> query fields a -> transaction (connection a)
    val listen : a ::: Type -> transaction (connection a) -> tunit
    val value : a ::: Type
                -> transaction (connection a) -> LinkedList.Signal.t a
end

(* functor Make(M : Input) : Output *)
(*     where con fields = M.fields *)
