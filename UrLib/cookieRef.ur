open Prelude

signature Input = sig
    con key :: Name
    con pulse :: Name
    con fields :: {Type}
    constraint [key] ~ [pulse]
    constraint [key] ~ fields
    constraint [pulse] ~ fields
    val fl : folder fields
    val sql : $(map sql_injectable fields)
end

functor Make(M : Input) = struct

open M

cookie key : int

table refs : ([key = int, pulse = Pulse.t] ++ fields)
    PRIMARY KEY {key}

structure P = Pulse.Make(struct
    con keys = [key = int]
    con pulse = pulse
    val tab = refs
    val seconds_refresh = 3600
    val seconds_timeout = 2 * seconds_refresh
end)

val fl = @Folder.cons [pulse] [_] ! fl

val sql = sql ++ {pulse = _}

fun setKey k =
    t <- now;
    setCookie key
              {Value = k.key,
               Expires = Some (addSeconds t 3600),
               Secure = True}

val getKey =
    Monad.mp (Monad.mp (Record.inj [key])) (getCookie key)

fun set xs =
    p <- Pulse.get;
    bind (@Sql.insertRandKeys ! _ fl sql refs (xs ++ {pulse = p})) setKey

val get =
    kq <- getKey;
    case kq of
        None => return None
      | Some k =>
        xsq <- P.lookup k;
        case xsq of
            None => return None
          | Some xs =>
            setKey k;
            return (Some xs)

end
