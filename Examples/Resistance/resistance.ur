open Prelude

con game =
    [NumPlayers = int,
     Round = int,
     Score = int,
     Attempt = int,
     Leader = int]

con team = [Team = list int]

val resps =
 fn [t] =>
    @@List.mp [{Response : _, Member : t}] [t] (proj [#Response])

fun nextLeader xs = mod (xs.Leader + 1) xs.NumPlayers

fun passed xs votes =
    let
        val vs : list bool = resps votes
    in
        error <xml>TODO</xml>
    end

fun succeeded xs actions =
    let
        val as : list bool = resps actions
    in
        error <xml>TODO</xml>
    end

structure F = Fsm.Make(struct
    type label = int
    val fsm =
        {Proposal = fn {State = xs : $game,
                        Effect = players} =>
                       make [#Voting] (xs ++ {Team = players}),
         Voting = fn {State = xs : $(game ++ team),
                      Effect = votes} =>
                     if passed xs votes then
                         make [#Mission] xs
                     else
                         make [#Proposal]
                              ({Attempt = xs.Attempt + 1} ++ projs xs),
         Mission = fn {State = xs : $(game ++ team),
                       Effect = actions} =>
                      make [#Proposal]
                           ({Round = xs.Round + 1,
                             Score = xs.Score + bit (succeeded xs actions),
                             Attempt = 0,
                             Leader = nextLeader xs}
                            ++ projs xs)}
end)
