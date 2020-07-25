(* A mechanism for having database rows expire when inactive. *)

include Prelude.Types

(* Type of "pulses", which is a timestamp. *)
type t

val sqlp : sql_injectable_prim t

(* Get the current pulse. *)
val get : transaction t

signature Input = sig
    con keys :: {Type}
    con vals :: {Type}
    con pulse :: Name
    constraint keys ~ vals
    constraint [pulse] ~ keys
    constraint [pulse] ~ vals
    val fl : folder keys
    val sql : $(map sql_injectable keys)
    table tab : (keys ++ vals ++ [pulse = t])
    val seconds_refresh : int
end

(* Periodically expunge the given table.
   Polls every [seconds_refresh] seconds.
   Removes rows that have not been looked up for [2 * seconds_refresh] seconds.
 *)
functor Make(M : Input) : sig
    (* Find a row and delay expunging it. *)
    val lookup : $M.keys -> transaction (option $M.vals)
end
