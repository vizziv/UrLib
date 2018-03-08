open Prelude

signature Input = sig
    type t
    con interface :: {Type -> Type}
    val t : $(map (fn op => op t) interface)
end

functor Make(M : Input) = M

con mk a = [Mk = fn t => a -> t]
con km a = [Km = fn t => t -> a]
con sql = [Sql = sql_injectable]
con sqlp = [Sqlp = sql_injectable_prim]

val mk = fn [a] => {Mk = identity}
val km = fn [a] => {Km = identity}
