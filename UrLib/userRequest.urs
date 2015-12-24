include Prelude.Types

functor Make(M : sig
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
end) : sig
    (* TODO: make this not break when called concurrently. *)
    val ask : {Group : M.group,
               Members : option (list M.member),
               Request : M.request}
              -> tunit
    (* Client: one-time setup (pick just one per user). *)
    val subscribeListener : {Group : M.group, Member : M.member}
                            -> $(map (fn h => (h.2 -> tunit) -> h.1 -> tunit)
                                     M.handlers)
                            -> tunit
    (* The source is set to [Some _] whenever a request is recieved and to
       [None] after each submission. *)
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     M.handlers)
    val subscribeSource : {Group : M.group, Member : M.member}
                          -> source (option submitRequest)
                          -> tunit
end
