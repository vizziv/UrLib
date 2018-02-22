open Prelude

signature Input = sig
    con fields :: {Type}
    con ref :: Name
    con pulse :: Name
    constraint [ref] ~ [pulse]
    constraint [ref] ~ fields
    constraint [pulse] ~ fields
    val fl : folder fields
    val sql : $(map sql_injectable fields)
end

functor Make(M : Input) = struct

open M

cookie ref : int

table refs : ([ref = int, pulse = Pulse.t] ++ fields)
    PRIMARY KEY {ref}

structure P = Pulse.Make(struct
    con keys = [ref = int]
    con pulse = pulse
    val tab = refs
    val seconds_refresh = 3600
    val seconds_timeout = 2 * seconds_refresh
end)

val fl = @Folder.cons [pulse] [_] ! fl

val sql = sql ++ {pulse = _}

fun setRef r =
    t <- now;
    setCookie ref
              {Value = r.ref,
               Expires = Some (addSeconds t 3600),
               Secure = True}

val getRef =
    Monad.mp (Monad.mp (Record.inj [ref])) (getCookie ref)

fun set xs =
    p <- Pulse.get;
    bind (@Sql.insertRandKeys ! _ fl sql refs (xs ++ {pulse = p})) setRef

val get =
    rq <- getRef;
    case rq of
        None => return None
      | Some r =>
        xsq <- P.lookup r;
        case xsq of
            None => return None
          | Some xs =>
            setRef r;
            return (Some xs)

end
