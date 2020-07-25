(* Make "newtypes", copies of types with restricted interfaces. *)

signature Input = sig
    type t
    con interface :: {Type -> Type}
    val t : $(map (fn tf => tf t) interface)
end

(* The output copy of the type is confined to the specified interface. *)
functor Make(M : Input) : Input where con interface = M.interface

(* Common interface features. *)
con mk a = [Mk = fn t => a -> t]
con km a = [Km = fn t => t -> a]
con sql = [Sql = sql_injectable]
con sqlp = [Sqlp = sql_injectable_prim]
con random = [Rng = Random.t]

(* Implementations of [mk] and [km] features. *)
val mk : a ::: Type -> {Mk : a -> a}
val km : a ::: Type -> {Km : a -> a}
