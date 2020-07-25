(* Use a randomly generated cookie to refer to server-side values. *)

include Prelude.Types

signature Input = sig
    con key :: Name
    con pulse :: Name
    con vals :: {Type}
    constraint [key] ~ [pulse]
    constraint [key] ~ vals
    constraint [pulse] ~ vals
    val fl : folder vals
    val sql : $(map sql_injectable vals)
end

functor Make(M : Input) : sig
    (* Set server-side values and set the cookie to refer to them. *)
    val set : $M.vals -> tunit
    (* Use the cookie to attempt to get server-side values. *)
    val get : transaction (option $M.vals)
end
