include Prelude.Types

signature Input = sig
    con key :: Name
    con pulse :: Name
    con vals :: {Type}
    constraint [key] ~ [pulse]
    constraint [key] ~ vals
    constraint [pulse] ~ vals
    val fl : folder vals
    val sql : $(map sql_injectable vals)
end

functor Make(M : Input) : sig
    val set : $M.vals -> tunit
    val get : transaction (option $M.vals)
end
