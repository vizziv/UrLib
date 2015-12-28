open Prelude

datatype role = Resistance | Spy

con game =
    [NumPlayers = int,
     Roles = list role,
     Round = int,
     Score = int,
     Attempt = int,
     Leader = int]

con team = [Team = list int]

fun countBit f : list {Response : bool, Member : int} -> int =
    List.foldl (fn resp acc => bit (f resp.Response) + acc) 0

val countTrue = countBit id
val countFalse = countBit not

fun new numPlayers =
    let
        fun randRoles acc numPlayers numSpies =
            if numPlayers = 0 then
                return acc
            else
                r <- rand;
                if mod r numPlayers < numSpies then
                    randRoles (Spy :: acc) (numPlayers - 1) (numSpies - 1)
                else
                    randRoles (Resistance :: acc) (numPlayers - 1) numSpies
    in
        roles <- randRoles [] numPlayers (case numPlayers of
                                              5 => 2
                                            | 6 => 2
                                            | 10 => 4
                                            | _ => 3);
        r <- rand;
        return {NumPlayers = numPlayers,
                Roles = roles,
                Round = 0,
                Score = 0,
                Attempt = 0,
                Leader = mod r numPlayers}
    end

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
    countFalse actions <= bit (xs.Round = 3 && xs.NumPlayers < 7)

fun team xs proposals =
    case proposals of
        {Member = player, Response = team} :: [] =>
        if player = xs.Leader then List.sort gt team else impossible
      | _ => impossible

val sm : StateMachine.t _ =
    {New =
      fn {State = xs, Effect = _ : list $([Response = unit] ++ _)} =>
         make [#Propose] xs,
     Propose =
      fn {State = xs, Effect = proposals} =>
         make [#Vote] ({Team = team xs proposals} ++ xs),
     Vote =
      fn {State = xs, Effect = votes} =>
         if passed xs votes then
             make [#Mission] xs
         else
             make [#Propose] ({Attempt = xs.Attempt + 1} ++ projs xs),
     Mission =
      fn {State = xs, Effect = actions} =>
         make [#Propose]
              ({Round = xs.Round + 1,
                Score = xs.Score + bit (succeeded xs actions),
                Attempt = 0,
                Leader = nextLeader xs}
               ++ projs xs)}

sequence gameLabels

datatype message =
         Proposing of int
       | Voting of list int
       | Votes of list {Player : int, Vote : bool}
       | Acting of list int
       | Actions of {Successes : int, Fails : int}

table games :
      {Game : int,
       NumPlayers : int,
       Started : bool,
       Channel : channel message}
          PRIMARY KEY Game

datatype players = All | Only of list int

fun dist [t] roles {Players = playersq, Resistance = rreq : t, Spy = sreq : t}
    : list {Member : int, Request : t} =
    let
        fun req i (role : role) =
            {Member = i,
             Request = case role of
                           Resistance => rreq
                         | Spy => sreq}
    in
        case playersq of
            All => List.mapi req roles
          | Only players =>
            List.mp (fn p => req p (case List.nth roles p of
                                        None => impossible
                                      | Some role => role))
                    players
    end

fun distSame [t] roles {Players = playersq, Request = req : t} =
    dist roles {Players = playersq, Resistance = req, Spy = req}

fun broadcast game message =
    {Channel = chan} <- oneRow1 (Sql.selectLookup games {Game = game});
    send chan message

fun request (game : int) =
    {New =
      fn xs =>
         let
             val spies = mapiPartial (fn i role =>
                                         case role of
                                             Resistance => None
                                           | Spy => Some i)
                                     xs.Roles
         in
             return (dist xs.Roles
                          {Players = All,
                           Resistance = None,
                           Spy = Some spies})
         end,
     Propose =
      fn xs =>
         broadcast game (Proposing xs.Leader);
         return ({Member = xs.Leader, Request = missionSize xs} :: []),
     Vote =
      fn xs =>
         broadcast game (Voting xs.Team);
         return (distSame xs.Roles {Players = All, Request = xs.Team}),
     Mission =
      fn xs =>
         broadcast game (Acting xs.Team);
         return (distSame xs.Roles {Players = Only xs.Team, Request = ()})}

open UserRequestStateMachine.Make(struct
    val sm = sm
    val request = request
end)

fun start gn =
    (xs : $game) <- new gn.NumPlayers;
    init {Group = gn.Game, State = make [#New] xs}
