open Prelude

con game =
    [NumPlayers = int,
     Round = int,
     Score = int,
     Attempt = int,
     Leader = int]

con team = [Team = list int]

val countTrue : list {Response : bool, Member : int} -> int =
    List.foldl (fn resp acc => bit resp.Response + acc) 0

fun nextLeader xs = mod (xs.Leader + 1) xs.NumPlayers

fun missionSize xs =
    case List.nth (case xs.NumPlayers of
                       5 => 2::3::2::3::3::[]
                     | 6 => 2::3::4::3::4::[]
                     | 7 => 2::3::3::4::4::[]
                     | _ => 3::4::4::5::5::[])
                  xs.Round of
        Some n => n
      | None => impossible

fun passed xs votes = countTrue votes > List.length votes

fun succeeded xs actions =
    countTrue actions > bit (xs.Round = 3 && xs.NumPlayers < 7)

fun team xs proposals =
    case proposals of
        {Member = player, Response = team} :: [] =>
        if player = xs.Leader then team else impossible
      | _ => impossible

structure F = Fsm.Make(struct
    type label = int
    val fsm =
        {Proposal = fn {State = xs : $game,
                        Effect = proposals} =>
                       make [#Voting] (xs ++ {Team = team xs proposals}),
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

structure U = UserRequest.Make(struct
    con handlers = _
    val mkCont =
end)
