include Prelude.Types

signature Input = sig
    con fields :: {Type}
    con ref :: Name
    con pulse :: Name
    constraint [ref] ~ [pulse]
    constraint [ref] ~ fields
    constraint [pulse] ~ fields
    val fl : folder fields
    val sql : $(map sql_injectable fields)
end

functor Make(M : Input) : sig
    val set : $M.fields -> tunit
    val get : transaction (option $M.fields)
end
