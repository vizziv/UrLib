open Prelude

type t (states :: {(Type * Type)}) =
    $(map (fn s => {State : s.1, Effect : s.2}
                   -> transaction (variant (map fst states)))
          states)

signature Types = sig
    con states :: {(Type * Type)}
    type label
    type state = variant (map fst states)
    type effect = variant (map snd states)
end

signature Input = sig
    include Types
    val fl_states : folder states
    val sql_label : sql_injectable_prim label
    val sm : label -> t states
end

signature Output = sig
    include Types
    val init : {Label : label, State : state} -> transaction state
    val step : {Label : label, Effect : effect} -> transaction (option state)
end

functor Make(M : Input) : Output
    where con states = M.states
    where type label = M.label = struct

open M

type state = variant (map fst M.states)
type effect = variant (map snd M.states)

table sms : {Label : label, State : serialized state} PRIMARY KEY Label

fun next label (x : state) (y : effect) =
    statevq
    <- @casesDiagTraverse [fst] [snd] [fn _ => _]
                          fl_states transaction_monad
                          (@mp [fn s => {State : s.1, Effect : s.2}
                                        -> transaction state]
                               [fn s => s.1 -> s.2 -> transaction state]
                               (fn [s] f state effect =>
                                   f {State = state, Effect = effect})
                               fl_states
                               (sm label))
                          x y;
    case statevq of
        None => return None
      | Some statev => return (Some (@casesGet fl_states statev))

fun init {Label = label, State = state} =
    Sql.insert sms {Label = label, State = serialize state};
    return state

fun step {Label = label, Effect = effect} =
    let
        val cond = Sql.lookup {Label = label}
    in
        {State = statez} <- oneRow1 (Sql.select sms cond);
        stateq <- next label (deserialize statez) effect;
        case stateq of
            None => return None
          | Some state =>
            Sql.update sms cond {State = serialize state};
            return (Some state)
    end

end
