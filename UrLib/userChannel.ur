open Prelude

con connection a = channel {Key : RandomKey.t, Message : serialized a}

con t a =
    {Send : a -> tunit,
     Connect : transaction (connection a)}

fun send [a] (t : t a) = t.Send
fun connect [a] (t : t a) = t.Connect

val recv : a ::: Type -> connection a -> transaction a
val spawnListener : a ::: Type -> connection a -> (a -> tunit) -> tunit

functor Make(M : sig type a end) = struct
    open M

    table connections : {User : User.t, Connection : connection a}
        PRIMARY KEY User

    table messages :
          {Key : RandomKey.t,
           User : User.t,
           Message : serialized a,
           When : time}
        PRIMARY KEY Key

    val getUser = Record.inj [#User] <<< User.get

    val connect =
        u <- getUser;
        rowq <- Sql.selectLookup connections u;
        case rowq of
            None =>
            cxn <- channel;
            Sql.insert connections (u ++ {Connection = Some cxn});
            return cxn
          | Some {Connection = Some cxn} => return cxn

    fun send x =
        u <- getUser;
        rowq <- Sql.selectLookup users u;
        case rowq of
            None =>
            n <- now;
            Sql.insertRandKeys messages
                               (u ++ {Message = serialize x, When = n})
          | Some {Connection = cxn} => Basis.send cxn msg

end
