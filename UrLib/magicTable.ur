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
     Fieldqs = @injqs ! flKeys flOthers ks}

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
    (* For debugging. *)
    val show_fields : $(map show fields)
    val labels_fields : $(map (fn _ => string) fields)
end

signature Output = sig
    include Types
    val insert : $fields -> tunit
    val update : write ::: {Type} -> Subset.t fields write
                 -> $write -> filter fields -> tunit
    val delete : filter fields -> tunit
    con connection :: {Type} -> Type
    val connect : read ::: {Type}
                  -> query fields read -> transaction (connection read)
    val listen : read ::: {Type} -> Subset.t fields read
                 -> connection read -> tunit
    val value : read ::: {Type}
                -> connection read -> LinkedList.signals $read
end

functor Make(M : Input) : Output
    where con fields = M.fields = struct

open M

con fieldqs = map option fields

datatype message =
    Insert of $fields
  | Update of {Values : $fieldqs, Filter : $fieldqs}
  | Delete of $fieldqs

table tab : fields

table listeners : ([chan = channel message] ++ fieldqs)

val flListeners : folder ([chan = channel message] ++ fieldqs) =
    @Folder.cons [chan] [channel message] ! (@Folder.mp fl)

val injs = @mp [_] [_] @@sql_prim fl sql_fields

val injsListeners
    : $(map sql_injectable ([chan = channel message] ++ fieldqs)) =
    {chan = _} ++ @mp [_] [compose _ _] @@sql_option_prim fl sql_fields

fun insert xs =
    let
        val xqs = @injqs ! fl Folder.nil xs
    in
        @Sql.insert fl injs tab xs;
        queryI1 (@Sql.selectCompat ! ! ! fl sql_fields _ listeners xqs)
                (fn (row : {chan : _}) =>
                    debug "sending insert";
                    send row.chan (Insert xs))
    end

fun update [write] (sub : Subset.t fields write)
           (xs : $write) (filter : filter fields) =
    Subset.elim
        (fn [others] [write ~ others] flWrite _ (pf : Eq.t fields _) =>
            let
                val xqs = Subset.injqs xs
            in
                @Sql.update ! flWrite (Subset.projs injs)
                            (Eq.cast pf [fn fs => sql_table fs []] tab)
                            (Eq.cast pf [filter] filter).Sql
                            xs;
                queryI1 (@Sql.selectCompat ! ! ! fl sql_fields _ listeners xqs)
                        (fn (row : {chan : _}) =>
                            debug "sending update";
                            send row.chan (Update {Values = xqs,
                                                   Filter = filter.Fieldqs}))
            end)

fun delete (filter : filter fields) =
    let
        val xqs = filter.Fieldqs
    in
        @Sql.delete tab filter.Sql;
        queryI1 (@Sql.selectCompat ! ! ! fl sql_fields _ listeners xqs)
                (fn (row : {chan : _}) =>
                    debug "sending delete";
                    send row.chan (Delete xqs))
    end

con connection (read :: {Type}) =
    {Channel : channel message, Source : LinkedList.sources $read}

fun connect [read] (q : query fields read) : transaction (connection read) =
    ch <- channel;
    @Sql.insert flListeners injsListeners
                listeners ({chan = ch} ++ q.Fieldqs);
    ll <- LinkedList.mk (fn [t] => query1 (q.Sql tab));
    return {Channel = ch, Source = ll}

fun listen [read] (sub : Subset.t fields read) (cxn : connection read) =
    let
        val ll = cxn.Source
        val flRead = @Subset.fl sub
        fun compat (xqs : $fieldqs) (ys : $read) =
            @foldR3 [eq] [option] [ident] [fn _ => bool]
                    (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest]
                        (_ : eq t) xq y acc =>
                        case xq of
                            None => acc
                          | Some x => acc && x = y)
                    True
                    flRead
                    (Subset.projs eq_fields) (Subset.projs xqs) ys
        fun modify (xqs : $fieldqs) (ys : $read) =
            @foldR2 [option] [ident] [record]
                    (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest]
                        xq y acc =>
                        case xq of
                            None => acc ++ {nm = y}
                          | Some x => acc ++ {nm = x})
                    {}
                    flRead
                    (Subset.projs xqs) ys
        fun go msg =
            case msg of
                Insert xs => LinkedList.insert (Subset.projs xs) ll
              | Update u =>
                LinkedList.update (modify u.Values) (compat u.Filter) ll
              | Delete xqs => LinkedList.delete (compat xqs) ll
        val debugPrint =
            @LinkedList.debugShow
                 (@Record.mkShow flRead
                                 (Subset.projs show_fields)
                                 (Subset.projs labels_fields))
                 cxn.Source
    in
        spawnListener (fn msg => go msg; debugPrint) cxn.Channel
    end

fun value [read] (cxn : connection read) = LinkedList.value cxn.Source

end
