open Prelude

(*
TODO:
  - Make [ask] not break when called concurrently.
  - Double check database cleanup needs.
*)

functor Make(M : sig
    con handlers :: {(Type * Type)}
    val fl : folder handlers
    type group
    val sql_group : sql_injectable group
    type member
    val sql_member : sql_injectable member
    type request = variant (map fst handlers)
    val mkCont : ({Members : list member, Request : request} -> tunit)
                 -> $(map (fn h => list {Member : member, Response : h.2} -> tunit)
                          handlers)
end) : sig
    (* Server: make a request, probably just once per group. *)
    val ask : {Group : M.group, Members : list M.member, Request : M.request}
              -> tunit
    (* Client: one-time setup with response functions. *)
    val listen : {Group : M.group, Member : M.member}
                 -> $(map (fn h => (h.2 -> tunit) -> h.1 -> tunit) M.handlers)
                 -> tunit
end = struct

open M

type job = int

type response = variant (map snd handlers)

sequence jobs

table users :
      {Group : group,
       Member : member,
       Channel : channel {Job : job, Request : request},
       Job : option job,
       Response : option response}
          PRIMARY KEY (Group, Member)

fun mkChannel user =
    chan <- channel;
    Sql.insert users (user ++ {Channel = chan, Job = None, Response = None});
    return chan

fun ask req =
    let
        val cond = (SQL T.Group = {[req.Group]}
                    AND {Sql.lookups (List.mp (snoc {} [#Member])
                                              req.Members)})
    in
        job <- nextval jobs;
        queryI1 (Sql.select1 users cond)
                (fn {Channel = chan} => send chan (sub req ++ {Job = job}));
        Sql.update users {Job = Some job} cond
    end

(* TODO: replace [--] usages with [sub]? *)

con respList t = list {Member : member, Response : t.2}

fun cont (user : {Group : group, Member : member}) job resp =
    let
        val job = {Job = Some job}
        val group = user -- #Member
        val member = user -- #Group
    in
        respsq <- query1' (SELECT users.Member, users.Response
                           FROM users
                           WHERE {Sql.lookup (group ++ job)}
                             AND NOT {Sql.lookup member})
                          (fn {Member = member, Response = respz} accq =>
                              bind accq
                                   (casesDiag [snd] [respList] [respList]
                                              (fn [t] resp acc =>
                                                  {Member = member,
                                                   Response = resp}
                                                  :: acc)
                                              (deserialize respz)))
                          (Some (casesMap [snd] [respList]
                                          (fn [t] resp =>
                                              ({Response = resp} ++ member)
                                              :: [])
                                          resp));
        case Option.mp (@@cases [map respList handlers] [_]
                                (mkCont (curry ask group)))
                       respsq of
            None =>
            Sql.update users {Response = Some resp} (Sql.lookup (user ++ job))
          | Some action =>
            Sql.update users {Job = None, Response = None} (Sql.lookup group);
            action

    end

fun answer user job resp = rpc (cont user job resp)

fun listen user listeners =
    let
        val ls1 =
            @mapNm [fn h => (h.2 -> tunit) -> h.1 -> tunit]
                   [fn hs h => (variant (map snd hs) -> tunit) -> h.1 -> tunit]
                   (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _ l0 f =>
                       l0 (fn resp => f (make [nm] resp)))
                   fl
                   listeners
        fun ls2 job =
            @mp [fn h => (response -> tunit) -> h.1 -> tunit]
                [fn h => h.1 -> tunit]
                (fn [h] l1 => l1 (answer user job))
                fl
                ls1
    in
        bind (rpc (mkChannel user))
             (spawnListener (fn {Job = job, Request = req} =>
                                (@@cases [map fst handlers] [_] (ls2 job) req)))
    end

end
