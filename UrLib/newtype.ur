open Prelude

signature Input = sig
    type t
    con interface :: {Type -> Type}
    val t : $(map (fn tf => tf t) interface)
end

functor Make(M : Input) = struct open M end

con mk a = [Mk = fn t => a -> t]
con km a = [Km = fn t => t -> a]
con sql = [Sql = sql_injectable]
con sqlp = [Sqlp = sql_injectable_prim]
con random = [Rng = Random.t]

val mk = fn [a] => {Mk = ident}
val km = fn [a] => {Km = ident}
