open Prelude

signature Params = sig
    con handlers :: {(Type * Type * Type)}
    val fl : folder handlers
    type group
    val sql_group : sql_injectable_prim group
    type member
    val sql_member : sql_injectable_prim member
    val eq_member : eq member
    con state (h :: (Type * Type * Type)) =
        (h.3, list {Member : member, Response : h.2})
    val sm : StateMachine.t (map state handlers)
    con translation (h :: (Type * Type * Type)) =
        h.3 -> transaction {Members : option (list member), Request : h.1}
    val translations : group -> $(map translation handlers)
end

functor Make(M : Params) = struct

open M

structure Sm = StateMachine.Make(struct
    con states = map state handlers
    val fl = @Folder.mp fl
    val sm = sm
end)

type req =
    {Members : option (list member),
     Request : variant (map fst3 handlers)}

fun translate (group : group) : Sm.state -> transaction req =
    compose (@casesFunctor (@Folder.mp fl)
                           (@Functor.compose _ (Functor.field [#Request])))
            (@casesMap [fn h :: (Type * Type * Type) => h.3]
                       [fn h :: (Type * Type * Type) =>
                           transaction {Members : option (list member),
                                        Request : h.1}]
                       fl
                       (translations group))

con ureqHandlers :: {(Type * Type)} = map (fn h => (h.1, h.2)) handlers

val flUreq : folder ureqHandlers =
    @@Folder.mp [fn h => (h.1, h.2)] [handlers] fl

fun mkCont (group : group) (ask : req -> tunit) =
    @mapNm0 [fn hs h => list {Member : member, Response : h.2} -> tunit]
            flUreq
            (fn [others ::_] [nm ::_] [h] [[nm] ~ others] _
                (pf : equal ureqHandlers ([nm = h] ++ others)) resps =>
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

structure Ureq = UserRequest.Make(struct
    con handlers = ureqHandlers
    val fl = flUreq
    type group = M.group
    type member = M.member
    val mkCont = mkCont
end)

end
