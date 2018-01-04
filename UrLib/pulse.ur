open Prelude

type t = int

val sqlp = _

val beat = Monad.mp toSeconds now

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

functor Make(M : Input) = struct

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
