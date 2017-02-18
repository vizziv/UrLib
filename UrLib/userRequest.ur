(*
TODO:
  - Make [ask] not break when called twice concurrently with the same group.
  - Deal with empty request lists.
  - Double check database cleanup needs.
  - Allow reconnection by calling [connect] again.
*)

open Prelude

signature Types = sig
    con handlers :: {(Type * Type)}
    type group
    type member
    type requests =
         variant (map (fn h => list {Member : member, Request : fst h}) handlers)
end

signature Input = sig
    include Types
    val fl_handlers : folder handlers
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
    val eq_member : eq member
    val cont : group
               -> (requests -> tunit)
               -> $(map (fn h =>
                            list {Member : member, Response : h.2} -> tunit)
                        handlers)
end

signature Output = sig
    include Types
    val ask : group -> requests -> tunit
    type connection
    val groupOf : connection -> group
    val memberOf : connection -> member
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     handlers)
    val connect : {Group : group, Member : member} -> transaction connection
    val listen : connection -> tunit
    val value : connection -> signal (option submitRequest)
end

functor Make(M : Input) : Output
    where con handlers = M.handlers
    where type group = M.group
    where type member = M.member = struct

open M

type job = int
type instance = variant (map (fn _ => job) handlers)
type request = variant (map fst handlers)
type response = variant (map snd handlers)

sequence jobs

table users :
      {Group : group,
       Member : member,
       Channel : channel {Job : int, Request : request},
       Key : int,
       Instance : option (serialized instance),
       Response : option (serialized response)}
          PRIMARY KEY (Group, Member)

type connection =
     {Group : group,
      Member : member,
      Key : int,
      Channel : channel {Job : int, Request : request},
      Source : source _}

val groupOf = Record.proj [#Group]
val memberOf = Record.proj [#Member]

fun connect user : transaction connection =
    chan <- channel;
    key <- rand;
    let
        val row = {Key = key, Channel = chan, Instance = None, Response = None}
                  ++ user
    in
        Sql.insert users row;
        src <- source None;
        return (Record.set row {Source = src})
    end

fun instantiate [tf] job variant =
    {Instance = Some (serialize (@casesMapU [tf] [fn _ => int] fl_handlers
                                            (fn [t] _ => job) variant))}

fun ask group (requests : requests) =
    let
        val reqs =
            @casesFunctor (@Folder.mp fl_handlers)
                          (@Functor.compose Functor.list
                                            (Functor.field [#Request]))
                          requests
        val members = List.mp (Record.proj [#Member]) reqs
        val cond = (SQL T.Group = {[group]}
                    AND {Sql.lookups (List.mp (Record.snoc {} [#Member]) members)})
        fun req member =
            case List.find (fn req => req.Member = member) reqs of
                None => impossible _LOC_
              | Some req => Record.projs req
    in
        job <- nextval jobs;
        let
            val instance = instantiate job requests
        in
            queryI1 (Sql.select users cond)
                    (fn {Member = member, Channel = chan} =>
                        send chan (req member ++ {Job = job}));
            Sql.update users cond instance
        end
    end

con respList t = list {Member : member, Response : t.2}

fun handle user job resp =
    let
        val instance = instantiate job resp
        val group = {Group = user.Group}
    in
        respsq <- query1' (Sql.selectLookup users (group ++ instance))
                          (fn {Member = member,
                               Key = key,
                               Response = respzq} accq =>
                              respz <- (if member = user.Member then
                                            if key = user.Key then
                                                Some (serialize resp)
                                            else
                                                None
                                        else
                                            respzq);
                              acc <- accq;
                              (@casesDiagU [snd] [respList] [respList]
                                           fl_handlers
                                           (fn [t] resp acc =>
                                               ({Member = member,
                                                 Response = resp})
                                               :: acc)
                                           (deserialize respz) acc))
                          (Some (@casesMapU [snd] [respList] fl_handlers
                                            (fn [t] _ => [])
                                            resp));
        case respsq of
            None =>
            Sql.updateLookup users
                             (user ++ instance)
                             {Response = Some (serialize resp)}
          | Some resps =>
            Sql.updateLookup users
                             group
                             {Instance = None, Response = None};
            @@cases [map respList handlers] [_]
                    (cont user.Group (ask user.Group)) resps
    end

fun answer (user : {Group : group, Member : member, Key : int}) job resp =
    rpc (handle user job resp)

fun subscribeListeners connection listeners =
    let
        val user = connection --- [Channel = _, Source = _]
        fun ls job =
            @mapNm [fn h => (h.2 -> tunit) -> h.1 -> tunit]
                   [fn hs h => h.1 -> tunit]
                   fl_handlers
                   (fn [others ::_] [nm ::_] [h]
                       [[nm] ~ others] _ (pf : Eq.t _ _)
                       l0 =>
                       l0 (fn resp =>
                              answer user job
                                     (Eq.cast pf [compose variant (map snd)]
                                              (make [nm] resp))))
                   listeners
    in
        spawnListener (fn {Job = job, Request = req} =>
                          (@@cases [map fst handlers] [_]
                                   (ls job)
                                   req))
                      connection.Channel
    end

type subReq (hs :: {(Type * Type)}) =
    variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1}) hs)

type submitRequest = subReq handlers

fun listen (connection : connection) =
    let
        fun f [others ::_] [nm ::_] [h] [[nm] ~ others] _ (pf : Eq.t _ _)
              (submit : h.2 -> tunit) (req : h.1) =
            let
                val src = connection.Source
            in
                set src
                    (Some (Eq.cast pf [subReq]
                                   (make [nm]
                                         {Submit =
                                           fn resp =>
                                              set src None;
                                              submit resp,
                                          Request = req})))
            end
        val listeners =
            @mapNm0 [fn _ h => (h.2 -> tunit) -> h.1 -> tunit] fl_handlers f
    in
        subscribeListeners connection listeners
    end

fun value connection = signal (connection.Source)

end
