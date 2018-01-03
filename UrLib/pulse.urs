include Prelude.Types

type t

val sqlp : sql_injectable_prim t

val beat : transaction t

signature Types = sig
    con keys :: {Type}
    con others :: {Type}
    con pulse :: Name
end

signature Input = sig
    include Types
    constraint keys ~ others
    constraint [pulse] ~ keys
    constraint [pulse] ~ others
    val fl : folder keys
    val sql : $(map sql_injectable keys)
    table tab : ([pulse = t] ++ keys ++ others)
    val seconds_refresh : int
    val seconds_timeout : int
end

signature Output = sig
    include Types
    constraint [pulse] ~ keys
    val ping : $([pulse = t] ++ keys) -> tunit
end

functor Make(M : Input) : Output
    where con keys = M.keys
    where con others = M.others
    where con pulse = M.pulse
