open Prelude

signature Input = sig
    con key :: Name
    con pulse :: Name
    con vals :: {Type}
    constraint [key] ~ [pulse]
    constraint [key] ~ vals
    constraint [pulse] ~ vals
    val fl : folder vals
    val sql : $(map sql_injectable vals)
end

functor Make(M : Input) = struct

open M

cookie key : RandomKey.t

table refs : ([key = RandomKey.t, pulse = Pulse.t] ++ vals)
    PRIMARY KEY {key}

structure P = Pulse.Make(struct
    con keys = [key = RandomKey.t]
    con pulse = pulse
    val tab = refs
    val seconds_refresh = 3600
    val seconds_timeout = 2 * seconds_refresh
end)

val fl = @Folder.cons [pulse] [_] ! fl

val sql = sql ++ {pulse = _}

fun setKey k =
    n <- now;
    setCookie key
              {Value = k.key,
               Expires = Some (addSeconds n 3600),
               Secure = True}

val getKey : transaction (option {key : RandomKey.t}) =
    @Mappable.mp Mappable.compose (Record.inj [key]) (getCookie key)

fun set xs =
    p <- Pulse.get;
    bind (@Sql.insertRandKeys ! ! _ _ _ fl sql refs (xs ++ {pulse = p})) setKey

val get =
    kq <- getKey;
    case kq of
        None => return None
      | Some k => setKey k; P.lookup k

end
