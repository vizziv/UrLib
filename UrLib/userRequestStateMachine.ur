open Prelude

signature Params = sig
    con handlers :: {(Type * Type * Type)}
    val fl : folder handlers
    type group
    val sql_group : sql_injectable_prim group
    type member
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    con state (i :: (Type * Type * Type)) =
        (i.3, list {Member : member, Response : i.2})
    val sm : StateMachine.t (map state handlers)
    con translation (i :: (Type * Type * Type)) =
        i.3 -> transaction {Members : option (list member), Request : i.1}
    val translations : group -> $(map translation handlers)
end

functor Make(M : Params) = struct

open M

structure Sm = StateMachine.Make(struct
    con states = map state handlers
    val sm = sm
end)

con req = _

fun translate (group : group) : Sm.state -> transaction req =
    compose (@casesFunctor (@Folder.mp fl) (@Functor.compose _ (Functor.field [#Request])))
            (@casesMap [fn h :: (Type * Type * Type) => _]
                       [fn h :: (Type * Type * Type) => _]
                       fl
                       (translations group))

fun mkCont (group : group) (ask : req -> tunit) =
    @mapNm0 [fn hs h => list {Member : member, Response : h.2} -> tunit]
            fl
            (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _
                (pf : equal handlers ([nm = h] ++ others)) resps =>
                stateq <- Sm.step {Label = group,
                                   Effect = castL pf
                                                  [fn hs =>
                                                      variant (map (fn h =>
                                                                       list {Member : member,
                                                                             Response : h.2})
                                                                   hs)]
                                                  (make [nm] resps)};
                case stateq of
                    None => impossible
                  | Some state => bind (translate group state) ask)

(* Adding this makes the comipler hang! *)
(*
structure Ureq = UserRequest.Make(struct
    con handlers = map (fn h => (h.1, h.2)) M.handlers
    val fl = @Folder.mp fl
    type group = group
    type member = member
    val mkCont = mkCont
end)
*)

end
