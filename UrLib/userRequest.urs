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
    val fl : folder handlers
    val sql_group : sql_injectable_prim group
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    val cont : group
               -> (requests -> tunit)
               -> $(map (fn h =>
                            list {Member : member, Response : h.2} -> tunit)
                        handlers)
end

signature Output = sig
    include Types
    (* Server-side initialization for each group. *)
    val ask : group -> requests -> tunit
    type connection
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     handlers)
    (* Server-side initialization for each user. *)
    val connect : {Group : group, Member : member} -> transaction connection
    (* Client-side initialization for each user.*)
    val listen : connection -> tunit
    (* The signal is set to [Some _] whenever a request is recieved and to
       [None] after each submission. *)
    val value : connection -> signal (option submitRequest)
end

functor Make(M : Input) : Output
    where con handlers = M.handlers
    where type group = M.group
    where type member = M.member
