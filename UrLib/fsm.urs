type t (states :: {(Type * Type)}) =
    $(map (fn s => {State : s.1, Effect : s.2} -> variant (map fst states))
          states)

functor Make(M : sig
    type label
    val sql_label : sql_injectable label
    con states :: {(Type * Type)}
    val fl : folder states
    val fsm : t states
end) : sig
    type state = variant (map fst M.states)
    type effect = variant (map snd M.states)
    val init : {Label : M.label, State : state} -> transaction state
    val step : {Label : M.label, Effect : effect} -> transaction (option state)
end
