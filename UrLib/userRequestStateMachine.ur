open Prelude

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
    type connection
    type submitRequest =
        variant (map (fn h => {Submit : h.2 -> tunit, Request : h.1})
                     handlers)
    val connect : {Group : group, Member : member} -> transaction connection
    val listen : connection -> tunit
    val value : connection -> signal (option submitRequest)
end

functor Make(M : Input) : Output
    where con handlerStates = M.handlerStates
    where type group = M.group
    where type member = M.member = struct

open M

structure Sm = StateMachine.Make(struct
    con states = M.states
    val fl = @Folder.mp fl
    val sm = sm
end)

type req =
    {Members : option (list member),
     Request : variant (map fst3 handlerStates)}

fun request (group : group) : Sm.state -> transaction req =
    compose (@casesFunctor (@Folder.mp fl)
                           (@Functor.compose _ (Functor.field [#Request])))
            (@casesMap [fn h :: (Type * Type * Type) => h.3]
                       [fn h :: (Type * Type * Type) =>
                           transaction {Members : option (list member),
                                        Request : h.1}]
                       fl
                       (mkRequest group))

con responses (hs :: {(Type * Type * Type)}) =
    variant (map (fn h => list {Member : member, Response : h.2}) hs)

fun mkCont (group : group) (ask : req -> tunit) =
    @mapNm0 [fn hs h => list {Member : member, Response : h.2} -> tunit] fl
            (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _
                (pf : equal handlerStates ([nm = h] ++ others)) resps =>
                stateq <- Sm.step {Label = group,
                                   Effect = castL pf [responses]
                                                  (make [nm] resps)};
                case stateq of
                    None => impossible
                  | Some state => bind (request group state) ask)

open UserRequest.Make(struct
    con handlers = M.handlers
    val fl = @Folder.mp fl
    type group = M.group
    type member = M.member
    val mkCont = mkCont
end)

fun init gs =
    state <- Sm.init (rename [#Group] [#Label] gs);
    bind (request gs.Group state) (curry ask (projs gs))

end
