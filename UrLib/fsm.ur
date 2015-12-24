(* Fsm = Finite State Machine *)

(* TODO: use better names than "state" repeated several times! *)

open Prelude

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
end = struct

open M

type state = variant (map fst M.states)
type effect = variant (map snd M.states)

table fsms : {Label : label, State : serialized state}

fun cont (x : state) (y : effect) =
    Option.mp (@casesGet fl)
              (@casesDiag [fst] [snd] [fn _ => state]
                          fl
                          (@mp [fn s => {State : s.1, Effect : s.2} -> state]
                               [fn s => s.1 -> s.2 -> state]
                               (fn [s] f state effect =>
                                   f {State = state, Effect = effect})
                               fl
                               fsm)
                          x y)

fun init {Label = label, State = state} =
    Sql.insert fsms {Label = label, State = serialize state};
    return state

fun step {Label = label, Effect = effect} =
    let
        val cond = Sql.lookup {Label = label}
    in
        {State = statez} <- oneRow1 (Sql.select1 fsms cond);
        case cont (deserialize statez) effect of
            None => return None
          | Some state =>
            Sql.update fsms {State = serialize state} cond;
            return (Some state)
    end

end
