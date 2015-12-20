include Prelude.Types

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
    val ask : {Group : M.group, Members : list M.member, Request : M.request}
              -> tunit
    (* Client: one-time setup with response functions. *)
    val listen : {Group : M.group, Member : M.member}
                 -> $(map (fn h => (h.2 -> tunit) -> h.1 -> tunit) M.handlers)
                 -> tunit
end
