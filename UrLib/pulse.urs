include Prelude.Types

type t

val sqlp : sql_injectable_prim t

val beat : transaction t

signature Input = sig
    con keys :: {Type}
    con others :: {Type}
    con pulse :: Name
    constraint keys ~ others
    constraint [pulse] ~ keys
    constraint [pulse] ~ others
    val fl : folder keys
    val sql : $(map sql_injectable keys)
    table tab : ([pulse = t] ++ keys ++ others)
    val seconds_refresh : int
    val seconds_timeout : int
end

functor Make(M : Input) : sig
    val ping : $([M.pulse = t] ++ M.keys) -> tunit
end
