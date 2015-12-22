(* Fsm = Finite State Machine *)

(* TODO: use better names than "state" repeated several times! *)

open Prelude

type t (effects :: {(Type * Type)}) (states :: {Type}) =
     $(map (fn s =>
               variant (map (fn e =>
                                {Arg : s -> e.1,
                                 Cont : e.2 -> s -> variant states})
                            effects))
           states)

functor Make(M : sig
    con effects :: {(Type * Type)}
    val fl_effects : folder effects
    con states :: {Type}
    val fl_states : folder states
    val fsm : t effects states
    type label
    val sql_label : sql_injectable label
end) : sig
    type arg = variant (map fst M.effects)
    type ans = variant (map snd M.effects)
    val init : {Label : M.label, State : variant M.states} -> transaction arg
    val step : {Label : M.label, Ans : ans} -> transaction (option arg)
end = struct

open M

type arg = variant (map fst effects)
type ans = variant (map snd effects)
con effect s e = {Arg : s -> e.1, Cont : e.2 -> s -> variant states}
con state s = variant (map (effect s) effects)

table fsms : {Label : M.label, State : serialized (variant states)}

val arg : variant states -> arg =
    cases (@mp [state] [fn s => s -> arg]
               (fn [s] (st : state s) (x : s) =>
                   @casesMap [effect s] [fst]
                             (fn [e] (ef : effect s e) =>
                                 proj [#Arg] ef x)
                             fl_effects
                             st)
               fl_states
               fsm)

fun cont (ans : ans) : variant states -> option (variant states) =
    cases (@mp [state] [fn s => s -> option (variant states)]
               (fn [s] (st : state s) (x : s) =>
                   Option.mp (@casesU fl_effects)
                             (@casesDiag [effect s] [snd]
	                                     [fn _ => variant states]
                                         (fn [e] (ef : effect s e) (y : e.2) =>
                                             proj [#Cont] ef y x)
                                         fl_effects
                                         st ans))
               fl_states
               fsm)

fun init {Label = label, State = state} =
    Sql.insert fsms {Label = label, State = serialize state};
    return (arg state)

fun step {Label = label, Ans = ans} =
    let
        val cond = Sql.lookup {Label = label}
    in
        {State = statez} <- oneRow1 (Sql.select1 fsms cond);
        case cont ans (deserialize statez) of
            None => return None
          | Some state =>
            Sql.update fsms {State = serialize state} cond;
            return (Some (arg state))
    end

end
