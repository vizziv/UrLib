open Prelude

con game =
    [NumPlayers = int,
     Spies = serialized (list int),
     Round = int,
     Score = int,
     Attempt = int,
     Leader = int]

con team = [Team = list int]

val countTrue : list {Response : bool, Member : int} -> int =
    List.foldl (fn resp acc => bit resp.Response + acc) 0

fun nextLeader (xs : $(game ++ team)) =
    mod (xs.Leader + 1) xs.NumPlayers

fun missionSize xs =
    case List.nth (case xs.NumPlayers of
                       5 => 2::3::2::3::3::[]
                     | 6 => 2::3::4::3::4::[]
                     | 7 => 2::3::3::4::4::[]
                     | _ => 3::4::4::5::5::[])
                  xs.Round of
        Some n => n
      | None => impossible

fun passed xs votes =
    countTrue votes > List.length votes

fun succeeded xs actions =
    countTrue actions > bit (xs.Round = 3 && xs.NumPlayers < 7)

fun team xs proposals =
    case proposals of
        {Member = player, Response = team} :: [] =>
        if player = xs.Leader then team else impossible
      | _ => impossible

con states = _

structure F = Fsm.Make(struct
    con states = states
    type label = int
    val fsm =
        {Proposal = fn {State = xs,
                        Effect = proposals} =>
                       make [#Voting] ({Team = team xs proposals} ++ xs),
         Voting = fn {State = xs,
                      Effect = votes} =>
                     if passed xs votes then
                         make [#Mission] xs
                     else
                         make [#Proposal]
                              ({Attempt = xs.Attempt + 1}
                               ++ projs xs),
         Mission = fn {State = xs,
                       Effect = actions} =>
                      make [#Proposal]
                           ({Round = xs.Round + 1,
                             Score = xs.Score + bit (succeeded xs actions),
                             Attempt = 0,
                             Leader = nextLeader xs}
                            ++ projs xs)}
end)

val translate : F.state -> transaction _ =
    compose
        casesExec
        (casesMap [fst] [snd]
                  {Proposal = fn xs =>
                                 return {Members = Some (xs.Leader :: []),
                                         Request = missionSize xs},
                   Voting = fn xs =>
                               return {Members = @@None [list int],
                                       Request = xs.Team},
                   Mission = fn xs =>
                                return {Members = Some xs.Team,
                                        Request = xs.Team}})

(* structure U = UserRequest.Make(struct *)
(*     con handlers = _ *)
(*     type group = int *)
(*     type member = int *)
(*     fun mkCont group ask = *)
(*         map0 [fn h => list {Member : member, Response : h.2} -> tunit] *)
(*              (fn [h] => ) *)
(* end) *)
