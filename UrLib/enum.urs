functor Make(M : sig
    con r :: {Unit}
    val fl : folder r
    val label : $(mapU string r)
end) : sig
    type t
    val eq : eq t
    val read : read t
    val show : show t
    val sqlp : sql_injectable_prim t
    val mk : $(mapU t M.r)
    val match : a ::: Type -> t -> $(mapU a M.r) -> a
end
