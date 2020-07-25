(* Randomly generated keys. *)
type t

(* They can be randomly generated, duh. *)
val rng : Random.t t

(* You can use them in SQL. *)
val sqlp : sql_injectable_prim t
