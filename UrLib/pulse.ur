open Prelude

type t = int

val sqlp = _

val get = Monad.mp toSeconds now

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

functor Make(M : Input) = struct

open M

task periodic seconds_refresh = fn () =>
    b <- get;
    Sql.delete tab (SQL ({[b]} - T.{pulse}) > {[seconds_timeout]})

fun beat ks =
    b <- get;
    if b - ks.pulse > seconds_refresh then
        @Sql.updateLookup ! _ _ ! fl sql tab (ks -- pulse) {pulse = b}
    else
        return ()

fun lookup ks =
    vsq <- oneOrNoRows1 (@Sql.selectLookup ! ! ! fl sql _ tab ks);
    case vsq of
        None => return None
      | Some vs =>
        beat (ks ++ (vs --- vals));
        return (Some (vs -- pulse))

end
