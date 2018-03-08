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

    table holding :
        {User : User.t,
         Message : serialized a,
         When : time}

    table sending :
        {User : User.t,
         Message : serialized a,
         Key : RandomKey.t}
        PRIMARY KEY User
        CONSTRAINT UNIQUE Key

    val getUser = Record.inj [#User] <<< User.get

    val connect =
        u <- getUser;
        cxn <- channel;
        _ <- Sql.setLookup connections (u ++ {Connection = cxn});
        return cxn

    fun send x =
        u <- getUser;
        n <- now;
         <- Monad.mp (fn x => x > 0) Sql.countLookup sending 
        Sql.insertRandKeys messages
                           (u ++ {Message = serialize x, When = n})
        queryI1 (Sql.lookup users u)
                ()

    fun ack msg

    val t = {Send = send, Connect = connect}

end
