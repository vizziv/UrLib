functor Make(M : sig
    con r :: {Unit}
    val fl_r : folder r
end) : sig
    type t
    val sqlp : sql_injectable_prim t
    val mk : $(mapU t M.r)
    val match : a ::: Type -> t -> $(mapU a M.r) -> a
end
