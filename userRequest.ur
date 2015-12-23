(*
TODO:
  - Make [ask] not break when called twice concurrently with the same group.
  - Double check database cleanup needs.
*)

open Prelude

functor Make(M : sig
    con handlers :: {(Type * Type)}
    val fl : folder handlers
    type group
    val sql_group : sql_injectable_prim group
    type member
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
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
type instance = variant (map (fn _ => job) handlers)
type response = variant (map snd handlers)

sequence jobs

val pkeyGroupMember = @primary_key [#Group] [[Member = _]] ! !
                                   {Group = sql_group, Member = sql_member}

table users :
      {Group : group,
       Member : member,
       Channel : channel {Job : int, Request : request},
       Instance : option (serialized instance),
       Response : option (serialized response)}
          PRIMARY KEY {{pkeyGroupMember}}

fun mkChannel user =
    chan <- channel;
    Sql.insert users
               (user ++ {Channel = chan, Instance = None, Response = None});
    return chan

fun instantiate [tf] job r =
    {Instance = Some (serialize (@casesMapU [tf] [fn _ => int]
                                            (fn [t] _ => job)
                                            fl
                                            r))}

fun ask req =
    let
        val cond = (SQL T.Group = {[req.Group]}
                    AND {Sql.lookups (List.mp (snoc {} [#Member])
                                              req.Members)})
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
        val group = user -- #Member
        val member = user -- #Group
        val instance = instantiate job resp
    in
        respsq <- query1' (Sql.selectLookup users (group ++ instance))
                          (fn {Member = member', Response = respzq} accq =>
                              respz <- (if @eq eq_member member' member.Member
                                        then Some (serialize resp)
                                        else respzq);
                              acc <- accq;
                              (@casesDiagU [snd] [respList] [respList]
                                           (fn [t] resp acc =>
                                               (member ++ {Response = resp})
                                               :: acc)
                                           fl
                                           (deserialize respz) acc))
                          (Some (@casesMapU [snd] [respList]
                                            (fn [t] _ => [])
                                            fl
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
                    (mkCont (curry ask group)) resps
    end

fun answer (user : {Group : group, Member : member}) job resp =
    rpc (cont user job resp)

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
                                (@@cases [map fst handlers] [_]
                                         (ls2 job)
                                         req)))
    end

(* type submitRequest = *)
(*      variant (map (fn h => {Submit: h.2 -> tunit, Request: h.1}) handlers) *)

(* fun signal (user : {Group : group, Member : member}) = *)
(*     src <- source (None : option submitRequest); *)
(*     listen user *)
(*            (@mapNm0 [fn _ h => (h.2 -> tunit) -> h.1 -> tunit] *)
(*                     (fn [others ::_] [nm ::_] [h] [[nm] ~ others] *)
(*                         (submit : h.2 -> tunit) (req : h.1) => *)
(*                         set src (Some (@@make [nm] [_] [others] *)
(*                                               {Submit = submit, *)
(*                                                Request = req}))) *)
(*                     fl); *)
(*     return (signal src) *)

end
