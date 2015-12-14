open Prelude

functor Make(M : sig
    con handlers :: {(Type * Type)}
    val fl : folder handlers
    type request = variant (map fst handlers)
    val mkCont : (request -> tunit) -> $(map (fn h => h.2 -> tunit) handlers)
end) : sig
    val listen : $(map (fn h => (h.2 -> tunit) -> h.1 -> tunit) M.handlers)
                 -> tunit
    val ask : M.request -> tunit
end = struct

open M

table channels :
      {Client : client, Channel : channel request}
          PRIMARY KEY Client

table requests :
      {Client : client, Request : serialized request}
          PRIMARY KEY Client

val channel =
    cli <- self;
    chan <- channel;
    Sql.insertRow channels {Client = cli, Channel = chan};
    queryI1 (Sql.selectWhereEq requests {Client = cli})
            (fn {Request = reqz} => send chan (deserialize reqz));
    Sql.deleteWhereEq requests {Client = cli};
    return chan

fun ask req =
    cli <- self;
    rowq <- oneOrNoRows1 (Sql.selectWhereEq channels {Client = cli});
    case rowq of
        None => Sql.insertRow requests {Client = cli, Request = serialize req}
      | Some {Channel = chan} => send chan req

val cont = @@cases [map snd handlers] [_] (mkCont ask)

fun answer resp = rpc (cont resp)

fun listen listeners =
    let
        val ls1 =
            @@mapNm [fn h => (h.2 -> tunit) -> h.1 -> tunit]
                    [fn hs h => (variant (map snd hs) -> tunit) -> h.1 -> tunit]
                    (fn [done ::_] [todo ::_] [nm ::_] [h] [[nm] ~ done] [done ++ [nm = h] ~ todo] l0 f =>
                        l0 (fn resp =>
                               f (@@make [nm] [h.2] [map snd (done ++ todo)] !
                                         resp)))
                    [handlers] fl listeners
        val ls2 =
            @@mp [fn h => (variant (map snd handlers) -> tunit)
                          -> h.1 -> tunit]
                 [fn h => h.1 -> tunit]
                 (fn [h] l1 => l1 answer)
                 [handlers] fl ls1
    in
        bind (rpc channel)
             (spawnListener (@@cases [map fst handlers] [_] ls2))
    end

end
