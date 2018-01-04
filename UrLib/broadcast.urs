include Prelude.Types

signature Input = sig
    type data
    type group
    type member
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
end

functor Make(M : Input) : sig
    val tell : M.group -> M.data -> tunit
    type connection
    (* Server-side initialization for each user. *)
    val connect :
        {Group : M.group, Member : M.member}
        -> transaction connection
    (* Client-side initialization for each user.*)
    val listen : (M.data -> tunit) -> connection -> tunit
end
