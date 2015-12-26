include Prelude.Types

signature Types = sig
    con handlers :: {(Type * Type)}
    type group
    type member
    type request = variant (map fst handlers)
end

signature Input = sig
    include Types
    val fl : folder handlers
    val sql_group : sql_injectable_prim group
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    val mkCont : group
                 -> ({Members : option (list member), Request : request}
                     -> tunit)
                 -> $(map (fn h =>
                              list {Member : member, Response : h.2} -> tunit)
                          handlers)
end

signature Output = sig
    include Types
    (* TODO: make this not break when called concurrently. *)
    val ask : {Group : group,
               Members : option (list member),
               Request : request}
              -> tunit
    (* Client: one-time setup (pick just one per user). *)
    val subscribeListener : {Group : group, Member : member}
                            -> $(map (fn h => (h.2 -> tunit) -> h.1 -> tunit)
                                     handlers)
                            -> tunit
    (* The source is set to [Some _] whenever a request is recieved and to
       [None] after each submission. *)
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     handlers)
    val subscribeSource : {Group : group, Member : member}
                          -> source (option submitRequest)
                          -> tunit
end

functor Make(M : Input) : Output
    where con handlers = M.handlers
    where type group = M.group
    where type member = M.member
