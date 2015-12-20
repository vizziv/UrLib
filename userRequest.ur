open Prelude

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
    (* TODO: make this not break when called concurrently. *)
    val ask : {Group : M.group, Members : list member, Request : request}
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
                    AND {lookups (List.mp (snoc {} [#Member]) req.Members)})
    in
        job <- nextVal jobs;
        queryI1 (Sql.select1 channels cond)
                (fn {Channel = chan} => send chan (sub req ++ {Job = job}));
        Sql.update users {Job = Some job} cond
    end

(* TODO: replace [--] usages with [sub]? *)

fun cont user resp =
    let
        val resp = mp [ident] [option] Some resp
        val job = {Job = resp.Job}
        val userjob = user ++ job
    in
        Sql.update users {Response = resp.Response} (Sql.lookup userjob);
        foo <- query1 (Sql.selectLookup users (userjob -- #Member))
                      ()
        @@cases [map snd handlers] [_] (mkCont (curry ask (user -- #Member)))
    end

fun answer user resp = rpc (cont user resp)

fun listen user listeners =
    let
        val ls1 =
            @mapNm [fn h => (h.2 -> tunit) -> h.1 -> tunit]
                   [fn hs h => (variant (map snd hs) -> tunit) -> h.1 -> tunit]
                   (fn [done ::_] [todo ::_] [nm ::_] [h]
                       [[nm] ~ done] [done ++ [nm = h] ~ todo]
                       l0 f =>
                       l0 (fn resp => f (make [nm] resp)))
                   fl
                   listeners
        val ls2 =
            @mp [fn h => (response -> tunit) -> h.1 -> tunit]
                [fn h => h.1 -> tunit]
                (fn [h] l1 => l1 (answer user))
                fl
                ls1
    in
        bind (rpc mkChannel)
             (spawnListener (@@cases [map fst handlers] [_] ls2))
    end

end
