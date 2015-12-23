open Prelude

structure M = struct
    con score = [Score = int]
    con team = [Team = int]
    type label = int
    val fsm =
        {Proposal = fn {State = xs : $score, Effect = players} =>
                       make [#Voting] (projs xs ++ {Team = players}),
         Voting = fn {State = xs : $(score ++ team), Effect = votes} =>
                     if votes > 3 then
                         make [#Mission] xs
                     else
                         make [#Proposal] (projs xs),
         Mission = fn {State = xs, Effect = actions} =>
                      make [#Proposal]
                           (if actions > 5 then
                                projs xs ++ {Score = xs.Score + 1}
                            else
                                projs xs)}
end

structure F = Fsm.Make(M)

structure U = UserRequest.Make(struct
    con handlers = map (M.)
