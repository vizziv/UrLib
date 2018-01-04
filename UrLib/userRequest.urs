include Prelude.Types

signature Types = sig
    con handlers :: {(Type * Type)}
    type group
    type member
    type requests =
        variant (map (fn h => list {Member : member, Request : h.1}) handlers)
end

signature Input = sig
    include Types
    val fl_handlers : folder handlers
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
    val eq_member : eq member
    val cont :
        group ->
        (requests -> tunit)
        -> $(map (fn h => list {Member : member, Response : h.2} -> tunit)
                 handlers)
end

functor Make(M : Input) : sig
    type connection
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     M.handlers)
    val ask : M.group -> M.requests -> tunit
    val connect :
        {Group : M.group, Member : M.member}
        -> transaction connection
    val groupOf : connection -> M.group
    val memberOf : connection -> M.member
    val listen : connection -> tunit
    val value : connection -> signal (option submitRequest)
end
