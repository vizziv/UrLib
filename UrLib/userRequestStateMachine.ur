open Prelude

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
    val init : {Group : group, State : variant (map fst states)} -> tunit
    type connection
    val groupOf : connection -> group
    val memberOf : connection -> member
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

(* For some reason, supplying the sketches of constructor functions helps with
type inference. *)

val translateRequest =
    compose (@casesTraverse [fn _ => _] [fn _ => _] fl _) request

val translateResponse =
    compose (@casesTraverse [fn _ => _] [fn _ => _] fl _) response

con responses (hs :: {(Type * Type * Type * Type)}) =
    variant (map (fn h => list {Member : member, Response : h.2}) hs)

fun cont (group : group) (ask : _ -> tunit) =
    @mapNm0 [fn hs h => list {Member : member, Response : h.2} -> tunit] fl
            (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _
                (pf : equal handlerStates ([nm = h] ++ others)) resps =>
                resp <- translateResponse group (castL pf [responses]
                                                       (make [nm] resps));
                stateq <- Sm.step {Label = group, Effect = resp};
                case stateq of
                    None => impossible _LOC_
                  | Some state => bind (translateRequest group state) ask)

open UserRequest.Make(struct
    con handlers = M.handlers
    val fl = @Folder.mp fl
    type group = M.group
    type member = M.member
    val cont = cont
end)

fun init gs =
    state <- Sm.init (rename [#Group] [#Label] gs);
    bind (translateRequest gs.Group state) (ask gs.Group)

end
