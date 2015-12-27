include UserRequestStateMachine.Output
    where con handlerStates =
        [Proposal = _,
         Vote = _,
         Mission = _]

val start : int -> tunit
