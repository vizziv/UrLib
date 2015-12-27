open Prelude

datatype role = Resistance | Spy

con game =
    [NumPlayers = int,
     Roles = serialized (list role),
     Round = int,
     Score = int,
     Attempt = int,
     Leader = int]

con team = [Team = list int]

val countTrue : list {Response : bool, Member : int} -> int =
    List.foldl (fn resp acc => bit resp.Response + acc) 0

fun new numPlayers =
    let
        fun randRoles acc numPlayers numSpies =
            if numPlayers = 0 then
                return acc
            else
                r <- rand;
                if mod r numPlayers < numSpies then
                    roles (Spy :: acc) (numPlayers - 1) (numSpies - 1)
                else
                    roles (Resistance :: acc) (numPlayers - 1) numSpies
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
    countTrue actions > bit (xs.Round = 3 && xs.NumPlayers < 7)

fun team xs proposals =
    case proposals of
        {Member = player, Response = team} :: [] =>
        if player = xs.Leader then team else impossible
      | _ => impossible

val sm : StateMachine.t _ =
    {New =
      fn {State = numPlayers, Effect = _ : list unit} =>
         make [#Reveal] numPlayers,
     Reveal =
      fn
     Proposal =
      fn {State = xs, Effect = proposals} =>
         make [#Vote] ({Team = team xs proposals} ++ xs),
     Vote =
      fn {State = xs, Effect = votes} =>
         if passed xs votes then
             make [#Mission] xs
         else
             make [#Proposal] ({Attempt = xs.Attempt + 1} ++ projs xs),
     Mission =
      fn {State = xs, Effect = actions} =>
         make [#Proposal]
              ({Round = xs.Round + 1,
                Score = xs.Score + bit (succeeded xs actions),
                Attempt = 0,
                Leader = nextLeader xs}
               ++ projs xs)}

sequence games

datatype message = Info of string | Chat of {Player : int, Message : string}

table games =
      {Game : int,
       NumPlayers : int,
       Started : bool,
       Channel : channel message}
          PRIMARY KEY Game

fun broadcast game message =
    {Channel = chan} <- Sql.selectLookup games {Game = game};
    send chan message

fun request (game : int) =
    {New =
      fn xs =>
         let
             val spies = List.filter (List.mapi (fn i _ => i) roles)
         in
             return {Members = spies, Request = spies}
         end,
     Proposal =
      fn xs =>
         return {Members = Some (xs.Leader :: []), Request = missionSize xs},
     Vote =
      fn xs =>
         return {Members = @@None [list int], Request = xs.Team},
     Mission =
      fn xs =>
         return {Members = Some xs.Team, Request = xs.Team}}

open UserRequestStateMachine.Make(struct
    val sm = sm
    val request = request
end)

fun start game =
    init {Group = game, State = }
