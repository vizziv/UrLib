open Prelude

con filter fields =
    {Sql : sql_exp [T = fields] [] [] bool,
     Fieldqs : $(map option fields)}

fun lookup [keys] [others] [keys ~ others]
           (flKeys : folder keys) (flOthers : folder others)
           (injs : $(map sql_injectable keys))
           (ks : $keys)
    : filter (keys ++ others) =
    {Sql = @Sql.lookup ! ! flKeys injs ks,
     Fieldqs = @map0 [_] (fn [t ::_] => None) flOthers
               ++ @mp [ident] [option] @@Some flKeys ks}

con query fields a =
    {Sql : sql_table fields [] -> sql_query [] [] [T = keep] [],
     Fieldqs : $(map option fields)}

fun select [keep] [drop] [keep ~ drop]
           (fl : folder keep)
           (filter : filter (keep ++ drop))
    : query (keep ++ drop) $keep =
    {Sql = fn tab => Sql.select tab filter.Sql,
     Fieldqs = filter.Fieldqs}

signature Types = sig
    con fields :: {Type}
end

signature Input = sig
    include Types
    con chan :: Name
    constraint [chan] ~ fields
    val fl : folder fields
    val sql_fields : $(map sql_injectable_prim fields)
end

signature Output = sig
    include Types
    val insert : $fields -> tunit
    val update : $(map option fields) -> filter fields -> tunit
    val delete : filter fields -> tunit
    con connection :: Type -> Type
    val connect : a ::: Type -> query fields a -> transaction (connection a)
    val listen : a ::: Type -> transaction (connection a) -> tunit
    val value : a ::: Type
                -> transaction (connection a) -> LinkedList.Signal.t a
end

functor Make(M : Input) (* : Output *)
    (* where con fields = M.fields *) = struct

open M

con fieldqs = map option fields

datatype message =
    Insert of $fields
  | Update of {Values : $fieldqs, Filter : $fieldqs}
  | Delete of $fieldqs

table tab : fields

table listeners : ([chan = channel message] ++ fieldqs)

con connection a =
    {Channel : channel message, Source : LinkedList.Source.t a}

val flListeners : folder ([chan = channel message] ++ fieldqs) =
    @Folder.cons [chan] [channel message] ! (@Folder.mp fl)

val injsListeners : $(map sql_injectable
                          ([chan = channel message] ++ fieldqs)) =
    {chan = _}
    ++ @mp [sql_injectable_prim] [compose sql_injectable option]
           @@sql_option_prim fl sql_fields

val connect =
 fn [a] =>
    let
        fun go [other ::_] [keep ::_] [other ~ keep]
               (q : isQuery fields a other keep) =
            ch <- channel;
            @Sql.insert flListeners injsListeners
                        listeners ({chan = ch} ++ q.Fieldqs);
            rows <- queryL1 (q.Sql tab);
            ll <- LinkedList.Source.mk (Eq.cast q.PfA [list] rows);
            return {Channel = ch, Source = ll}
    in
        fn q => @@exDisj_elim [isQuery fields a] q [_] go
    end

(* fun listen [a] (cxn : connection a) = *)
(*     let *)
(*         val ll = cxn.Source *)
(*         val compat = *)
(*             @foldR3 [eq] [option] [ident] [fn _ => bool] *)
(*                     (fn [nm ::_] [t ::_] [rest ::_] [_~_] *)
(*                         (_ : eq t) xq y acc => *)
(*                         case xq of *)
(*                             None => acc *)
(*                           | Some x => acc && x = y) *)
(*                     True *)
(*                     fl eq_fields *)
(*         fun go msg = *)
(*             case msg of *)
(*                 Insert xs => LinkedList.Source.insert xs ll *)
(*               | Update u => *)
(*                 LinkedList.Source.update (modify u.Values) (compat u.Filter) ll *)
(*               | Delete xqs => LinkedList.Source.delete (compat xqs) ll *)
(*     in *)
(*         spawnListener go cxn.Channel *)
(*     end *)

fun value [a] (cxn : connection a) = LinkedList.Source.value cxn.Source

end
