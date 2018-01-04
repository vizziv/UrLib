(* From the official Ur/Web demo. *)

include Prelude.Types

type t

val new : transaction t
val render : t -> signal xbody
val write : t -> string -> tunit
