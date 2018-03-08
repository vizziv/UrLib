type t = RandomKey.t

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
