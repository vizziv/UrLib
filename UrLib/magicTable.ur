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

con query fields keep =
    {Sql : sql_table fields [] -> sql_query [] [] [T = keep] [],
     Fieldqs : $(map option fields)}

fun select [keep] [drop] [keep ~ drop]
           (fl : folder keep)
           (filter : filter (keep ++ drop))
    : query (keep ++ drop) keep =
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
    val eq_fields : $(map eq fields)
    val sql_fields : $(map sql_injectable_prim fields)
end

signature Output = sig
    include Types
    val insert : $fields -> tunit
    val update : $(map option fields) -> filter fields -> tunit
    val delete : filter fields -> tunit
    con connection :: {Type} -> Type
    val connect : keep ::: {Type} ->
                  query fields keep -> transaction (connection keep)
    val listen : keep ::: {Type} -> connection keep -> tunit
    val value : keep ::: {Type}
                -> transaction (connection keep) -> LinkedList.Signal.t $keep
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

fun connect [keep] (q : query fields keep) =
    ch <- channel;
    @Sql.insert flListeners injsListeners
                listeners ({chan = ch} ++ q.Fieldqs);
    ll <- LinkedList.Source.mk (fn [t] => query (q.Sql tab));
    return {Channel = ch, Source = ll}

fun listen [keep] (sub : Subset.t fields keep) (cxn : connection $keep) =
    let
        val ll = cxn.Source
        fun compat (xqs : $fieldqs) (ys : $keep) =
            @Subset.elim
                sub
                (fn [drop] [keep ~ drop] flKeep _ pf =>
                    @foldR3 [eq] [option] [ident] [fn _ => bool]
                            (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest]
                                (_ : eq t) xq y acc =>
                                case xq of
                                    None => acc
                                  | Some x => acc && x = y)
                            True
                            flKeep
                            (projs (@Eq.cast pf [compose record (map eq)]
                                             eq_fields))
                            (projs (Eq.cast pf [compose record (map option)]
                                            xqs))
                            ys)
        fun modify (xqs : $fieldqs) (ys : $keep) =
            @Subset.elim
                sub
                (fn [drop] [keep ~ drop] flKeep _ pf =>
                    @foldR2 [option] [ident] [record]
                            (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest]
                                xq y acc =>
                                case xq of
                                    None => acc ++ {nm = y}
                                  | Some x => acc ++ {nm = x})
                            {}
                            flKeep
                            (projs (Eq.cast pf [compose record (map option)]
                                            xqs))
                            ys)
        fun go msg =
            case msg of
                Insert xs => LinkedList.Source.insert (Subset.projs xs) ll
              | Update u =>
                LinkedList.Source.update (modify u.Values) (compat u.Filter) ll
              | Delete xqs => LinkedList.Source.delete (compat xqs) ll
    in
        spawnListener go cxn.Channel
    end

fun value [a] (cxn : connection a) = LinkedList.Source.value cxn.Source

end
