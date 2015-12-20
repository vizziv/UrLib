(*****************************)
(* Fsm: Finite State Machine *)
(*****************************)

open Prelude

type request (handlers :: {(Type * Type)}) b a =
     variant (map (fn h => {Arg : a -> h.1, Cont : a -> h.2 -> b}) handlers)

type fsm (handlers :: {(Type * Type)}) (states :: {Type}) =
     $(map (request handlers (variant states)) states)


(***********************)
(* Example: Resistance *)
(***********************)

con score = [Score = _]
con team = [Team = int]

val resistance : fsm [Propose = _, Vote = _, Go = _] _ =
    {Proposal =
     make [#Propose]
          {Arg = id,
           Cont = fn (xs : $score) players =>
                     make [#Voting] (sub xs ++ {Team = players})},
     Voting =
     make [#Vote]
          {Arg = id,
           Cont = fn (xs : $(score ++ team)) votes =>
                     if votes > 3 then
                         make [#Mission] xs
                     else
                         make [#Proposal] (sub xs)},
     Mission =
     make [#Go]
          {Arg = id,
           Cont = fn xs actions =>
                     make [#Proposal]
                          (if actions > 5 then
                               sub xs ++ {Score = xs.Score + 1}
                           else
                               sub xs)}}
