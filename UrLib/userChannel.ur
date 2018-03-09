open Prelude

functor Make(M : sig type a end) = struct

con user = [User = User.t]
con message = [Message = serialized M.message]
con channel = [Channel = Basis.channel $(message ++ verify)]
con state = [State = serialized M.state]

table users : user ++ state
    PRIMARY KEY User

table messages : user ++ message ++ [Verify = RandomVerify.t, Time : time]
    PRIMARY KEY Verify
    FOREIGN KEY User REFERENCES users(User)
      ON UPDATE CASCADE ON DELETE CASCADE

table channels : user ++ channel
    PRIMARY KEY User
    FOREIGN KEY User REFERENCES users(User)
      ON UPDATE CASCADE ON DELETE CASCADE

type connection = $(channel ++ state)

val getUser = Monad.exec {User = User.get}

fun selectNextMsgv user =
    (SELECT Msgv.Message, Msgv.Verify
     FROM messages AS Msgv
     WHERE Msgv.User = {[user.User]}
     ORDER BY Msgv.Time ASC
     LIMIT 1)

fun selectNextMsgvCxn user =
    (SELECT Cxn.Connection, Msgv.Message, Msgv.Verify
     FROM channels AS Cxn JOIN held AS Msg
       ON Cxn.User = Msgv.User
     WHERE Msgv.User = {[user.User]}
     ORDER BY Msgv.Time ASC
     LIMIT 1)

val connect =
    user <- getUser;
    chan <- Monad.exec {Channel = channel};
    (* Restarts from last state if present or from initial state if not. *)
    state <- Sql.selectAndSetLookup users user {State = serialize M.init} {};
    _ <- Sql.setLookup channels user chan;
    queryI1 (selectNextMsgv user)
            (Record.concat state >>> Basis.send chan.Channel);
    return (chan ++ state)

fun send msg =
    user <- getUser;
    msgv <- Monad.exec {Message = return (serialize msg), Verify = Random.gen};
    r <- Sql.insertLookup sent user msgv;
    (* TODO: below *)
    case r of
      (* Send this message. *)
        Inserted =>
        queryI1 (Sql.selectLookup channels user)
                (fn {Channel = c} =>
                    Sql.updateLookup states user {State = M.step msg s};
                    Basis.send c msgv)
      (* Another message is sending, so hold this one. *)
      | NotInserted =>
        time <- Monad.exec {Time = now};
        Sql.insert held (user ++ msgv -- #Verify ++ time)

fun ack verify =
    user <- getUser;
    isVerified <- Sql.existsLookup messages (user ++ verify);
    when isVerified
         (Sql.deleteLookup messages (user ++ verify);
          queryI (selectNextMsgvCxn user)
                 (fn row => Basis.send row.Cxn.Channel row.Msgv))

fun listen cxn action =
    spawnLoop (fn () =>
                  msgv <- recv cxn.Channel;
                  spawn (rpc ack (msgv --- message));
                  action msg.Message)

end
