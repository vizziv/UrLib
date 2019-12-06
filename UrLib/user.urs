functor Make(Empty : sig end) : sig
    type t
    val sqlp : sql_injectable_prim t
    val get : transaction t

    functor Expunger(M : sig
        con user :: Name
        con fields :: {Type}
        constraint [user] ~ fields
        table tab : ([user = t] ++ fields)
    end) : sig end
end
