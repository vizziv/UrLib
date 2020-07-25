(* Broadcast messages to members of a group. *)

include Prelude.Types

signature Input = sig
    (* Type of messages to send. *)
    type message
    (* Type of identifiers of groups that can recieve messages. *)
    type group
    (* Type of identifiers of members of groups. *)
    type member
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
end

functor Make(M : Input) : sig
    (* Send a group a message. *)
    val tell : M.group -> M.message -> tunit
    type connection
    (* Server-side initialization for each user. *)
    val connect :
        {Group : M.group, Member : M.member}
        -> transaction connection
    (* Client-side initialization for each user.*)
    val listen : (M.message -> tunit) -> connection -> tunit
end
