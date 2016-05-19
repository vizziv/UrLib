include Prelude.Types

signature Types = sig
    con handlerStates :: {(Type * Type * Type * Type)}
    include UserRequest.Types
        where con handlers = map (fn h => (h.1, h.2)) handlerStates
    include StateMachine.Types
        where con states = map (fn h => (h.3, h.4)) handlerStates
        where type label = group
    type requestTranslations =
        $(map (fn h =>
                  h.3 -> transaction (list {Member : member, Request : h.1}))
              handlerStates)
    type responseTranslations =
        $(map (fn h =>
                  list {Member : member, Response : h.2} -> transaction h.4)
              handlerStates)
end

signature Input = sig
    include Types
    val fl : folder handlerStates
    val sql_group : sql_injectable_prim group
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    val sm : group -> StateMachine.t states
    val request : group -> requestTranslations
    val response : group -> responseTranslations
end

signature Output = sig
    include Types
    (* Server-side initialization for each group. *)
    val init : {Group : group, State : variant (map fst states)} -> tunit
    type connection
    val groupOf : connection -> group
    val memberOf : connection -> member
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     handlers)
    (* Server-side initialization for each user. *)
    val connect : {Group : group, Member : member} -> transaction connection
    (* Client-side initialization for each user.*)
    val listen : connection -> tunit
    (* The signal is set to [Some _] whenever a request is recieved and to
       [None] after each submission. *)
    val value : connection -> signal (option submitRequest)
end

functor Make(M : Input) : Output
    where con handlerStates = M.handlerStates
    where type group = M.group
    where type member = M.member
