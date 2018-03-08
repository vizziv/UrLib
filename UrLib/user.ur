structure User = Newtype.Make(struct
    open Newtype
    type t = int
    con interface = sqlp ++ random
    val t = {Sqlp = _, Rng = _}
end)

type t = User.t

val sqlp = User.t.Sqlp
val _ = User.t.Rng

cookie user : t

table users : {User : t, Pulse : Pulse.t}
    PRIMARY KEY User

structure P = Pulse.Make(struct
    con pulse = #Pulse
    val tab = users
    val seconds_refresh = 3600
    val seconds_timeout = 2 * seconds_refresh
end)

val newUser =
    p <- Pulse.get;
    {User = u} <- Sql.insertRandKeys users {Pulse = p};
    return u

fun verify uq =
    case uq of
        None => newUser
      | Some u =>
        q <- P.lookup {User = u};
        case q of
            None => newUser
          | Some () => return u

val get =
    u <- bind (getCookie user) verify;
    setCookie user {Value = u, Expires = None, Secure = True};
    return u
