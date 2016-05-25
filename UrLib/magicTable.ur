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

con isQuery fields a keep drop =
    [keep ~ drop]
    => {Sql : sql_table fields [] -> sql_query [] [] [T = keep] [],
        Fieldqs : $(map option fields),
        Fl : folder keep,
        PfA : Eq.t $keep a,
        PfFields : Eq.t (keep ++ drop) fields}

con query fields a = exDisj (isQuery fields a)

fun select [keep] [drop] [keep ~ drop]
           (fl : folder keep)
           (filter : filter (keep ++ drop))
    : query (keep ++ drop) $keep =
    @exDisj_intro [isQuery (keep ++ drop) $keep] [keep] [drop] !
                  (fn [_~_] =>
                      {Sql = fn tab => Sql.select tab filter.Sql,
                       Fieldqs = filter.Fieldqs,
                       Fl = fl,
                       PfFields = Eq.refl,
                       PfA = Eq.refl})

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
    {Chan : channel message, Source : LinkedList.Source.t a}

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
            LinkedList.Source.mk (Eq.cast q.PfA [list] rows)
    in
        fn q => @@exDisj_elim [isQuery fields a] q [_] go
    end

(* fun value [a] (cxn : connection a) = LinkedList.Source.value cxn.Source *)

end
