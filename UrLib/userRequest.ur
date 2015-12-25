(*
TODO:
  - Make [ask] not break when called twice concurrently with the same group.
  - Double check database cleanup needs.
*)

open Prelude

signature Params = sig
    con handlers :: {(Type * Type)}
    val fl : folder handlers
    type group
    val sql_group : sql_injectable_prim group
    type member
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    type request = variant (map fst handlers)
    val mkCont : group
                 -> ({Members : option (list member), Request : request}
                     -> tunit)
                 -> $(map (fn h =>
                              list {Member : member, Response : h.2} -> tunit)
                          handlers)
end

functor Make(M : Params) : sig
    val ask : {Group : M.group,
               Members : option (list M.member),
               Request : M.request}
              -> tunit
    val subscribeListener : {Group : M.group, Member : M.member}
                             -> $(map (fn h => (h.2 -> tunit) -> h.1 -> tunit)
                                      M.handlers)
                            -> tunit
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                M.handlers)
    val subscribeSource : {Group : M.group, Member : M.member}
                          -> source (option submitRequest)
                          -> tunit
end = struct

open M

type job = int
type instance = variant (map (fn _ => job) handlers)
type response = variant (map snd handlers)

sequence jobs

val pkeyGroupMember = @primary_key [#Group] [[Member = _]] ! !
                                   {Group = sql_group,
                                    Member = sql_member}

table users :
      {Group : group,
       Member : member,
       Channel : channel {Job : int, Request : request},
       Key : int,
       Instance : option (serialized instance),
       Response : option (serialized response)}
          PRIMARY KEY {{pkeyGroupMember}}

fun init user : transaction {Channel : channel _, Key : int} =
    chan <- channel;
    key <- rand;
    let
        val clientInfo = user ++ {Key = key, Channel = chan}
    in
        Sql.insert users (clientInfo ++ {Instance = None, Response = None});
        return (projs clientInfo)
    end

fun instantiate [tf] job variant =
    {Instance = Some (serialize (@casesMapU [tf] [fn _ => int] fl
                                            (fn [t] _ => job) variant))}

fun ask req =
    let
        val cond' = (SQL T.Group = {[req.Group]})
        val cond =
            case req.Members of
                None => cond'
              | Some members =>
                (SQL {cond'}
                 AND {Sql.lookups (List.mp (snoc {} [#Member]) members)})
    in
        job <- nextval jobs;
        let
            val instance = instantiate job req.Request
        in
            queryI1 (Sql.select1 users cond)
                    (fn {Channel = chan} =>
                        send chan (projs req ++ {Job = job}));
            Sql.update users instance cond
        end
    end

con respList t = list {Member : member, Response : t.2}

fun cont user job resp =
    let
        val group = projs user
        val member = projs user
        val instance = instantiate job resp
    in
        respsq <- query1' (Sql.selectLookup users (group ++ instance))
                          (fn {Member = member',
                               Key = key',
                               Response = respzq} accq =>
                              respz <- (if not (key' = user.Key) then
                                            None
                                        else if member' = member.Member then
                                            Some (serialize resp)
                                        else
                                            respzq);
                              acc <- accq;
                              (@casesDiagU [snd] [respList] [respList] fl
                                           (fn [t] resp acc =>
                                               (member ++ {Response = resp})
                                               :: acc)
                                           (deserialize respz) acc))
                          (Some (@casesMapU [snd] [respList] fl
                                            (fn [t] _ => [])
                                            resp));
        case respsq of
            None =>
            Sql.update users
                       {Response = Some (serialize resp)}
                       (Sql.lookup (user ++ instance))
          | Some resps =>
            Sql.update users
                       {Instance = None, Response = None}
                       (Sql.lookup group);
            @@cases [map respList handlers] [_]
                    (mkCont group.Group (curry ask group)) resps
    end

fun answer (user : {Group : group, Member : member, Key : int}) job resp =
    rpc (cont user job resp)

fun subscribeListener (user : {Group : group, Member : member}) listeners =
    let
        fun ls key job =
            @mapNm [fn h => (h.2 -> tunit) -> h.1 -> tunit]
                   [fn hs h => h.1 -> tunit]
                   fl
                   (fn [others ::_] [nm ::_] [h]
                       [[nm] ~ others] _ (pf : equal _ _)
                       l0 =>
                       l0 (fn resp =>
                              answer (user ++ {Key = key}) job
                                     (castL pf [fn hs => variant (map snd hs)]
                                            (make [nm] resp))))
                   listeners
    in
        {Channel = chan, Key = key} <- rpc (init user);
        spawnListener (fn {Job = job, Request = req} =>
                          (@@cases [map fst handlers] [_]
                                   (ls key job)
                                   req))
                      chan
    end

type subReq (hs :: {(Type * Type)}) =
    variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1}) hs)

type submitRequest = subReq handlers

fun subscribeSource (user : {Group : group, Member : member})
                    (src : source (option submitRequest)) =
    let
        fun f [others ::_] [nm ::_] [h] [[nm] ~ others] _ (pf : equal _ _)
              (submit : h.2 -> tunit) (req : h.1) =
            let
                val cast =
                    castL pf
                          (* Using [subReq] hangs the compiler! *)
                          [fn hs =>
                              variant (map (fn h =>
                                               {Submit : h.2 -> tunit,
                                                Request : h.1})
                                           hs)]
            in
                set src
                    (Some (cast (make [nm]
                                      {Submit = fn resp =>
                                                   set src None;
                                                   submit resp,
                                       Request = req})))
            end
        val listeners =
            @mapNm0 [fn _ h => (h.2 -> tunit) -> h.1 -> tunit] fl f
    in
        subscribeListener user listeners
    end

end
