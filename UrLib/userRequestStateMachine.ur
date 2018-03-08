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
    val fl_handlerStates : folder handlerStates
    val sqlp_group : sql_injectable_prim group
    val sqlp_member : sql_injectable_prim member
    val eq_member : eq member
    val sm : group -> StateMachine.t states
    val request : group -> requestTranslations
    val response : group -> responseTranslations
end

functor Make(M : Input) = struct

open M

structure Sm = StateMachine.Make(struct
    con states = M.states
    val fl_states = @Folder.mp fl_handlerStates
    val sm = sm
end)

(* For some reason, supplying the sketches of constructor functions helps with
type inference. *)

val translateRequest =
    request >>> (@Cases.traverse [fn _ => _] [fn _ => _] fl_handlerStates _)

val translateResponse =
    response >>> (@Cases.traverse [fn _ => _] [fn _ => _] fl_handlerStates _)

con responses (hs :: {(Type * Type * Type * Type)}) =
    variant (map (fn h => list {Member : member, Response : h.2}) hs)

fun cont (group : group) (ask : _ -> tunit) =
    @mapNm0 [fn h => list {Member : member, Response : h.2} -> tunit]
            fl_handlerStates
            (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _
                (pf : Eq.t _ _) resps =>
                resp <- translateResponse group (Eq.cast pf [responses]
                                                         (make [nm] resps));
                stateq <- Sm.step {Label = group, Effect = resp};
                case stateq of
                    None => impossible _LOC_
                  | Some state => bind (translateRequest group state) ask)

open UserRequest.Make(struct
    con handlers = M.handlers
    val fl_handlers = @Folder.mp fl_handlerStates
    type group = M.group
    type member = M.member
    val cont = cont
end)

fun init gs =
    state <- Sm.init (Record.rename [#Group] [#Label] gs);
    bind (translateRequest gs.Group state) (ask gs.Group)

end
