open Prelude

open Fsm.Make(struct
    con effects = [Propose = _, Vote = _, Go = _]
    con score = [Score = int]
    con team = [Team = int]
    type label = int
    val fsm =
        {Proposal =
         make [#Propose]
              {Arg = id,
               Cont = fn players (xs : $score) =>
                         make [#Voting] (projs xs ++ {Team = players})},
         Voting =
         make [#Vote]
              {Arg = id,
               Cont = fn votes (xs : $(score ++ team)) =>
                         if votes > 3 then
                             make [#Mission] xs
                         else
                             make [#Proposal] (projs xs)},
         Mission =
         make [#Go]
              {Arg = id,
               Cont = fn actions xs =>
                         make [#Proposal]
                              (if actions > 5 then
                                   projs xs ++ {Score = xs.Score + 1}
                               else
                                   projs xs)}}
end)
