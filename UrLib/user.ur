type t = RandomKey.t

val sqlp = _

cookie user : t

table users : {User : t, Pulse : Pulse.t}
    PRIMARY KEY User

val seconds_refresh = 3600

structure P = Pulse.Make(struct
    con pulse = #Pulse
    val tab = users
    val seconds_refresh = seconds_refresh
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

functor Expunger(M : sig
    con user :: Name
    con fields :: {Type}
    constraint [user] ~ fields
    table tab : ([user = t] ++ fields)
end) = struct

open M

task periodic seconds_refresh =
 fn () =>
    queryI1 (SELECT tab.{user}
             FROM tab LEFT JOIN users ON tab.{user} = users.User
             WHERE users.User = NULL)
            (Sql.deleteLookup tab)

end
