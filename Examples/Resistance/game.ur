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

val sm =
    {Proposal = fn {State = xs,
                    Effect = proposals} =>
                   make [#Voting] ({Team = team xs proposals} ++ xs),
     Voting = fn {State = xs,
                  Effect = votes} =>
                 if passed xs votes then
                     make [#Mission] xs
                 else
                     make [#Proposal]
                          ({Attempt = xs.Attempt + 1} ++ projs xs),
     Mission = fn {State = xs,
                   Effect = actions} =>
                  make [#Proposal]
                       ({Round = xs.Round + 1,
                         Score = xs.Score + bit (succeeded xs actions),
                         Attempt = 0,
                         Leader = nextLeader xs}
                        ++ projs xs)}

structure Sm = StateMachine.Make(struct
    type label = int
    val sm = sm
end)

type req = _

fun translate (group : int) : Sm.state -> transaction req =
    compose (@casesFunctor _ (@Functor.compose _ (Functor.field [#Request])))
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

con handlers :: {(Type * Type)} = _

fun mkCont (group : int) (ask : req -> tunit) =
    @mapNm0 [fn hs h => list {Member : int, Response : h.2} -> tunit]
            (_ : folder handlers)
            (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _
                (pf : equal handlers ([nm = h] ++ others)) resps =>
                stateq <- Sm.step {Label = group,
                                   Effect = castL pf
                                                  [fn hs =>
                                                      variant (map (fn h =>
                                                                       list {Member : int,
                                                                             Response : h.2})
                                                                   hs)]
                                                  (make [nm] resps)};
                case stateq of
                    None => impossible
                  | Some state => bind (translate group state) ask)

structure Ureq = UserRequest.Make(struct
    con handlers = handlers
    type group = int
    type member = int
    val mkCont = mkCont
end)
