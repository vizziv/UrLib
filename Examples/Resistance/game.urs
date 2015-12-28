include Prelude.Types

include UserRequestStateMachine.Output

val start : {Game : int, NumPlayers : int} -> tunit

datatype message =
         Proposing of int
       | Voting of list int
       | Votes of list {Player : int, Vote : bool}
       | Acting of list int
       | Actions of {Successes : int, Fails : int}
