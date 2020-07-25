(* Class for types whose values can be randomly generated. *)
class t :: Type -> Type

(* Specify how to randomly generate a value from a random [int]. *)
val mk : a ::: Type -> (int -> a) -> t a

(* Generate a random value. *)
val gen : a ::: Type -> t a -> transaction a

val int : t int
