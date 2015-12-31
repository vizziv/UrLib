(* From the official Ur/Web demo. *)

type t

val new : transaction t
val render : t -> signal xbody
val write : t -> string -> transaction unit
