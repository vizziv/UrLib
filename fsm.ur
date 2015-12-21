(* Fsm = Finite State Machine *)

open Prelude

type request (effects :: {(Type * Type)}) b a =
     variant (map (fn e => {Arg : a -> e.1, Cont : a -> e.2 -> b}) effects)

type fsm (effects :: {(Type * Type)}) (states :: {Type}) =
     $(map (request effects (variant states)) states)


(* Example: the Resistance card game (just a sketch for now). *)

con score = [Score = _]
con team = [Team = int]

val resistance : fsm [Propose = _, Vote = _, Go = _] _ =
    {Proposal =
     make [#Propose]
          {Arg = id,
           Cont = fn (xs : $score) players =>
                     make [#Voting] (projs xs ++ {Team = players})},
     Voting =
     make [#Vote]
          {Arg = id,
           Cont = fn (xs : $(score ++ team)) votes =>
                     if votes > 3 then
                         make [#Mission] xs
                     else
                         make [#Proposal] (projs xs)},
     Mission =
     make [#Go]
          {Arg = id,
           Cont = fn xs actions =>
                     make [#Proposal]
                          (if actions > 5 then
                               projs xs ++ {Score = xs.Score + 1}
                           else
                               projs xs)}}
