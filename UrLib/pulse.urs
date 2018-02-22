include Prelude.Types

type t

val sqlp : sql_injectable_prim t

val get : transaction t

signature Input = sig
    con keys :: {Type}
    con vals :: {Type}
    con pulse :: Name
    constraint keys ~ vals
    constraint [pulse] ~ keys
    constraint [pulse] ~ vals
    val fl : folder keys
    val sql : $(map sql_injectable keys)
    table tab : (keys ++ vals ++ [pulse = t])
    val seconds_refresh : int
    val seconds_timeout : int
end

functor Make(M : Input) : sig
    val beat : $(M.keys ++ [M.pulse = t]) -> tunit
    val lookup : $M.keys -> transaction (option $M.vals)
end
