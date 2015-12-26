type t (states :: {(Type * Type)}) =
    $(map (fn s => {State : s.1, Effect : s.2} -> variant (map fst states))
          states)

signature Types = sig
    con states :: {(Type * Type)}
    type label
    type state = variant (map fst states)
    type effect = variant (map snd states)
end

signature Input = sig
    include Types
    val fl : folder states
    val sql_label : sql_injectable label
    val sm : t states
end

signature Output = sig
    include Types
    val init : {Label : label, State : state} -> transaction state
    val step : {Label : label, Effect : effect} -> transaction (option state)
end

functor Make(M : Input) : Output
    where con states = M.states
    where type label = M.label
