open Prelude

type t = int

val sqlp = _

val beat = Monad.mp toSeconds now

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
    where con pulse = M.pulse = struct

open M

task periodic seconds_refresh = fn () =>
    b <- beat;
    Sql.delete tab (SQL ({[b]} - T.{pulse}) > {[seconds_timeout]})

fun ping ks =
    b <- beat;
    if b - ks.pulse > seconds_refresh then
        @Sql.updateLookup ! _ _ ! fl sql tab (ks -- pulse) {pulse = b}
    else
        return ()

end
