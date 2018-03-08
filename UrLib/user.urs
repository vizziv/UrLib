type t

val sqlp : sql_injectable_prim t

val get : transaction t

(* Creates a task that removes rows with expired users. *)
functor Expunger(M : sig
    con user :: Name
    con fields :: {Type}
    constraint [user] ~ fields
    table tab : ([user = t] ++ fields)
end) : sig end
