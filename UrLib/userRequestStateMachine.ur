open Prelude

signature Params = sig
    con info :: {(Type * Type * Type)}
    con handlers = map (fn i => (i.1, i.2)) info
    type group
    val sql_group : sql_injectable_prim group
    type member
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    con states =
        map (fn i => (i.3, list {Member : member, Response : i.2})) info
    con sm : StateMachine.t states
    val translate : group -> 
end

functor Make(M : sig
    con handlers :: {(Type * Type * Type)}
    type group
    type member
