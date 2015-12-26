include Prelude.Types

signature Types = sig
    con handlerStates :: {(Type * Type * Type)}
    include UserRequest.Types
        where con handlers = map (fn h => (h.1, h.2)) handlerStates
    include StateMachine.Types
        where con states =
	          map (fn h => (h.3, list {Member : member, Response : h.2}))
                  handlerStates
        where type label = group
    type translations =
        $(map (fn h =>
                h.3 -> transaction {Members : option (list member),
                                    Request : h.1})
              handlerStates)
end

signature Input = sig
    include Types
    val fl : folder handlerStates
    val sql_group : sql_injectable_prim group
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    val sm : StateMachine.t states
    val mkRequest : group -> translations
end

signature Output = sig
    include Types
    val init : {Group : group, State : variant (map fst states)} -> tunit
    type submitRequest =
         variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                      handlerStates)
    val subscribeSource : {Group : group, Member : member}
                          -> source (option submitRequest)
                          -> tunit

end

functor Make(M : Input) : Output
    where con handlerStates = M.handlerStates
    where type group = M.group
    where type member = M.member
