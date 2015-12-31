open Prelude

(* Back End *)

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
        fun randRoles acc unassigned spiesNeeded =
            if unassigned = 0 then
                return acc
            else
                r <- rand;
                if mod r unassigned < spiesNeeded then
                    randRoles (Spy :: acc) (unassigned - 1) (spiesNeeded - 1)
                else
                    randRoles (Resistance :: acc) (unassigned - 1) spiesNeeded
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

fun missionRequest xs =
    case List.nth (case xs.NumPlayers of
                       5 => 2::3::2::3::3::[]
                     | 6 => 2::3::4::3::4::[]
                     | 7 => 2::3::3::4::4::[]
                     | _ => 3::4::4::5::5::[])
                  xs.Round of
        Some n => {NumPlayers = xs.NumPlayers, MissionSize = n}
      | None => impossible

fun passed xs votes =
    countFalse votes < xs.NumPlayers / 2

fun succeeded xs actions =
    countFalse actions <= bit (xs.Round = 3 && xs.NumPlayers < 7)

fun team xs proposals =
    case proposals of
        {Member = player, Response = team} :: [] =>
        if player = xs.Leader then List.sort gt team else impossible
      | _ => impossible

val sm : StateMachine.t _ =
    {New = fn {State = xs, Effect = ()} => make [#Propose] xs,
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
         let
             val score = xs.Score + bit (succeeded xs actions)
         in
             if score >= 3 then
                 make [#Done] {Winner = Resistance, Roles = xs.Roles}
             else if xs.Round - score >= 3 then
                 make [#Done] {Winner = Spy, Roles = xs.Roles}
             else
                 make [#Propose]
                      ({Round = xs.Round + 1,
                        Score = score,
                        Attempt = 0,
                        Leader = nextLeader xs}
                       ++ projs xs)
         end,
     Done = fn {State = xs, Effect = ()} => make [#Done] xs}

datatype members = All | Subset of list int

fun dist [t] roles {Members = membersq, Resistance = rreq : t, Spy = sreq : t}
    : list {Member : int, Request : t} =
    let
        fun req i (role : role) =
            {Member = i,
             Request = case role of
                           Resistance => rreq
                         | Spy => sreq}
    in
        case membersq of
            All => List.mapi req roles
          | Subset members =>
            List.mp (fn p => req p (case List.nth roles p of
                                        None => impossible
                                      | Some role => role))
                    members
    end

fun distSame [t] roles {Members = membersq, Request = req : t} =
    dist roles {Members = membersq, Resistance = req, Spy = req}

type group = int

sequence groups

datatype message =
         Proposing of int
       | Voting of list int
       | Votes of list {Member : int, Response : bool}
       | Acting of list int
       | Actions of {Successes : int, Fails : int}
       | Victory of {Winner : role, Roles : list role}

table games :
      {Group : group,
       NumPlayers : int,
       Started : bool,
       Channel : channel message}
          PRIMARY KEY Group

fun broadcast group message =
    {Channel = chan} <- oneRow1 (Sql.selectLookup games {Group = group});
    send chan message

fun finish group =
    Sql.deleteLookup games {Group = group}

val spies =
    mapiPartial (fn i role =>
                    case role of
                        Resistance => None
                      | Spy => Some i)

fun request (group : group) =
    {New =
      fn xs =>
         return (dist xs.Roles
                      {Members = All,
                       Resistance = None,
                       Spy = Some (spies xs.Roles)}),
     Propose =
      fn xs =>
         broadcast group (Proposing xs.Leader);
         return ({Member = xs.Leader,
                  Request = missionRequest xs}
	             :: []),
     Vote =
      fn xs =>
         broadcast group (Voting xs.Team);
         return (distSame xs.Roles {Members = All, Request = xs.Team}),
     Mission =
      fn xs =>
         broadcast group (Acting xs.Team);
         return (distSame xs.Roles {Members = Subset xs.Team, Request = ()}),
     Done =
      fn xs =>
         broadcast group (Victory xs);
         finish group;
         return (distSame xs.Roles {Members = All, Request = ()})}

fun response (group : group) =
    {New = fn (_ : list {Member : _, Response : unit}) => return (),
     Propose = @@return [_] [list {Member : _, Response : _}] _,
     Vote =
      fn votes =>
         broadcast group (Votes votes);
         return votes,
     Mission =
      fn actions =>
         broadcast group (Actions {Successes = countTrue actions,
                                   Fails = countFalse actions});
         return actions,
     Done = fn (_ : list {Member : _, Response : void}) => return ()}

open UserRequestStateMachine.Make(struct
    val sm = sm
    val request = request
    val response = response
end)

fun join groupq =
    let
        val userChan =
            case groupq of
                None =>
                group <- nextval groups;
                chan <- channel;
                Sql.insert games
                           {Group = group,
                            NumPlayers = 1,
                            Started = False,
                            Channel = chan};
                return ({Group = group, Member = 0}, chan)
              | Some group =>
                {NumPlayers = n, Started = started, Channel = chan}
                <- oneRow1 (Sql.selectLookup games {Group = group});
                if started then
                    impossible
                else
                    Sql.updateLookup games {Group = group} {NumPlayers = n+1};
                    return ({Group = group, Member = n}, chan)
    in
        (user, chan) <- userChan;
        connection <- connect user;
        return {Player = user.Member, Connection = connection, Channel = chan}
    end

fun start connection =
    let
        val group = {Group = groupOf connection}
    in
        {NumPlayers = n, Started = started}
        <- oneRow1 (Sql.selectLookup games group);
        if started then
            impossible
        else
            Sql.updateLookup games group {Started = True};
            (xs : $game) <- new n;
            init (group ++ {State = make [#New] xs})
    end

(* Front End. *)

fun groupsX [ctx] [inp] (f : group -> xml ctx inp []) =
    queryX1 (Sql.select games (SQL TRUE)) (compose f proj1)

val showSpies = compose Misc.showList spies

val show_message : show message =
    let
        val showVotes =
            compose Misc.stringList
                    (List.mp (fn {Member = player, Response = vote} =>
                                 show player ^ ": "
                                 ^ if vote then "approve" else "reject"))
        fun showRole role =
            case role of
                Resistance => "Resistance"
              | Spy => "Spy"
        fun showMessage message =
            case message of
                Proposing player =>
                "Player " ^ show player ^ " proposing a team."
              | Voting team =>
                "Voting on team " ^ Misc.showList team ^ "."
              | Votes votes =>
                "Votes are " ^ showVotes votes ^ "."
              | Acting team =>
                "Team " ^ Misc.showList team ^ " going on mission."
              | Actions {Successes = s, Fails = f} =>
                "There were " ^ show s ^ " successes and " ^ show f ^ "fails."
              | Victory {Winner = w, Roles = rs} =>
                showRole w ^ " victory! The spies were " ^ showSpies rs ^ "."
    in
        mkShow showMessage
    end

fun formNew sr = <xml>
  {[case sr.Request of
        None => "You're a loyal Resistance member."
      | Some spies =>
        "You're a Spy. The spies are " ^ Misc.showList spies ^ "."]}
</xml>

fun formPropose sr =
    srcs <- List.tabulateM (fn _ => source 0.0) sr.Request.MissionSize;
    let
        val numPlayers = sr.Request.NumPlayers
        val sgl =
            team <- List.mapM (compose (Monad.mp round) signal) srcs;
            if Misc.distinct team
               && minimum numPlayers team >= 0
               && maximum 0 team < numPlayers then
                return (Ui.submitButton {Value = "Propose",
                                         Onclick = sr.Submit team})
            else
                return <xml></xml>
    in
        return <xml>
          {List.mapX (fn src => <xml>
            <cnumber source={src}
                     min={0.0}
                     max={float (numPlayers - 1)}
                     step={1.0}/>
          </xml>) srcs}
          <dyn signal={sgl}/>
        </xml>
    end

fun formBool submit {True = nameT, False = nameF} =
    Ui.submitButtons ({Value = nameT, Onclick = submit True},
                      {Value = nameF, Onclick = submit False})

fun render srvq =
    case srvq of
        None => <xml>Waiting....</xml>
      | Some srv =>
        match srv
              {New = formNew,
               Propose =
                fn sr => <xml>
                  <active code={formPropose sr}/>
                </xml>,
               Vote =
                fn sr => <xml>
                  Proposed team is {[Misc.showList sr.Request]}.<br/>
                  {formBool sr.Submit {True = "Approve", False = "Reject"}}
                </xml>,
               Mission =
                fn sr => <xml>
                  You're going on the mission.<br/>
                  {formBool sr.Submit {True = "Success", False = "Fail"}}
                </xml>,
               Done = fn _ => <xml>Game over.</xml>}

fun play groupq () : transaction page =
    buffer <- Buffer.new;
    {Player = player, Connection = connection, Channel = chan} <- join groupq;
    return <xml>
      <body onload={listen connection;
                    spawnListener (compose (Buffer.write buffer) show) chan}>
        <h1>Resistance</h1>
        <h3>Player {[player]}</h3>
        <dyn signal={Monad.mp render (value connection)}/>
        <hr/>
        <dyn signal={Buffer.render buffer}/>
      </body>
    </xml>


val menu : transaction page =
    groups <- groupsX (fn group => <xml>
      <li>
        <submit action={play (Some group)} value={"Join game #" ^ show group}/>
      </li>
    </xml>);
    return <xml>
      <body>
        <form>
          <ul>
            {groups}
            <li>
              <submit action={play None} value={"Create new game"}/>
            </li>
          </ul>
        </form>
      </body>
    </xml>
