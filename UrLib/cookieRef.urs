include Prelude.Types

signature Input = sig
    con key :: Name
    con pulse :: Name
    con fields :: {Type}
    constraint [key] ~ [pulse]
    constraint [key] ~ fields
    constraint [pulse] ~ fields
    val fl : folder fields
    val sql : $(map sql_injectable fields)
end

functor Make(M : Input) : sig
    val set : $M.fields -> tunit
    val get : transaction (option $M.fields)
end
