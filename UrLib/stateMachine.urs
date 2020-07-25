(* Type of state machines:
   given the current state and an action taken, transition to a new state.
 *)
type t (states :: {(Type * Type)}) =
    $(map (fn s => {State : s.1, Action : s.2}
                   -> transaction (variant (map fst states)))
          states)

signature Types = sig
    con states :: {(Type * Type)}
    type label
    type state = variant (map fst states)
    type action = variant (map snd states)
end

signature Input = sig
    include Types
    val fl_states : folder states
    val sql_label : sql_injectable_prim label
    val sm : label -> t states
end

functor Make(M : Input) : sig
    (* Create a state machine with a given [label]. *)
    val init :
        {Label : M.label, State : M.state}
        -> transaction M.state
    (* Takes the given [action] in the [label]ed state machine.
       Returns [None] when the action doesn't match the state.
     *)
    val step :
        {Label : M.label, Action : M.action}
        -> transaction (option M.state)
end
