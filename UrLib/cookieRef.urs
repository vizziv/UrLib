include Prelude.Types

signature Types = sig
    con fields :: {Type}
end

signature Input = sig
    include Types
    con ref :: Name
    constraint [ref] ~ fields
    val fl : folder fields
    val sql : $(map sql_injectable fields)
end

signature Output = sig
    include Types
    val set : $fields -> tunit
    val get : transaction (option $fields)
end

functor Make(M : Input) : Output
    where con fields = M.fields
