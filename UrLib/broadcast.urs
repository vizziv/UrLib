include Prelude.Types

signature Types = sig
    type data
    type group
    type member
end

signature Input = sig
    include Types
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
end

signature Output = sig
    include Types
    val tell : group -> data -> tunit
    type connection
    (* Server-side initialization for each user. *)
    val connect : {Group : group, Member : member} -> transaction connection
    (* Client-side initialization for each user.*)
    val listen : (data -> tunit) -> connection -> tunit
end

functor Make(M : Input) : Output
    where type data = M.data
    where type group = M.group
    where type member = M.member
