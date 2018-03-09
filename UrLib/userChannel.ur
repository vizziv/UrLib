open Prelude

con message a = {Key : RandomKey.t, Message : serialized a}

con connection a = transaction (message a)

con t a = {Send : a -> tunit, Connect : transaction (connection a)}

fun send [a] (t : t a) = t.Send
fun connect [a] (t : t a) = t.Connect

val recv = fun [a] => @@ident [connection a]
val spawnListener = basis

functor Make(M : sig type a end) = struct

open M

table connections : {User : User.t, Channel : channel (message a)}
    PRIMARY KEY User

table holding :
    {User : User.t,
     Message : serialized a,
     Time : time}

table sending :
    {User : User.t,
     Message : serialized a,
     Key : RandomKey.t}
    PRIMARY KEY User

val getUser = Monad.exec {User = User.get}

val connect =
    user <- getUser;
    chan <- channel;
    _ <- Sql.setLookup connections user {Channel = chan};
    return chan

fun sendFromHolding user =

fun sendFromSending user =


fun send msg =
    user <- getUser;
    msg <- Monad.exec {Message = return (serialize msg), Key = Random.gen};
    r <- Sql.insertLookup sending user msg;
    case r of
        Inserted =>
        queryI1 (Sql.selectLookup connections user)
                (fn {Channel = chan} => Basis.send chan msg)
      | NotInserted =>
        time <- Monad.exec {Time = now};
        Sql.insert holding (user ++ msg -- #Key ++ time)

fun ack key =
    user <- getUser;
    keyCorrect <- Sql.existsLookup connections (user ++ key);
    when keyCorrect (sendFromHolding user)

val t = {Send = send, Connect = connect}

end
