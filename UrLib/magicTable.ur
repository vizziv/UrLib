open Prelude

con filter fields =
    {Sql : sql_exp [T = fields] [] [] bool,
     Fieldqs : $(map option fields)}

con query fields keep =
    {Sql : sql_table fields [] -> sql_query [] [] [T = fields] [],
     Fieldqs : $(map option fields)}

con connection fields read =
    {Listen : tunit,
     Value : LinkedList.signals $read}

con t fields =
    {Insert : $fields -> tunit,
     Update : write ::: {Type} -> Subset.t fields write
              -> $write -> filter fields -> tunit,
     Delete : filter fields -> tunit,
     Connect : read ::: {Type} -> Subset.t fields read
               -> query fields read -> transaction (connection fields read)}

fun lookup [keys] [others] [keys ~ others]
           (fl_keys : folder keys) (fl_others : folder others)
           (sql_keys : $(map sql_injectable keys))
           (ks : $keys)
    : filter (keys ++ others) =
    {Sql = @Sql.lookup ! ! fl_keys sql_keys ks,
     Fieldqs = @Record.injqs ! fl_keys fl_others ks}

fun select [keep] [drop] [keep ~ drop]
           (fl_keep : folder keep)
           (filter : filter (keep ++ drop))
    : query (keep ++ drop) keep =
    {Sql = fn tab => Sql.select tab filter.Sql,
     Fieldqs = filter.Fieldqs}

fun insert [fields] t = t.Insert

fun update [write] [others] [write ~ others]
           fl_write fl_others
           (t : t (write ++ others)) =
    @@t.Update [write] (@Subset.intro ! fl_write fl_others)

fun delete [fields] t = t.Delete

fun connect [read] [others] [read ~ others]
            fl_read fl_others
            (t : t (read ++ others)) (q : query (read ++ others) read) =
    @@t.Connect [read] (@Subset.intro ! fl_read fl_others) q

fun listen [fields] [read] cxn = cxn.Listen

fun value [read] [others] [read ~ others] cxn = cxn.Value

signature Types = sig
    con fields :: {Type}
end

signature Input = sig
    include Types
    con chan :: Name
    constraint [chan] ~ fields
    val fl : folder fields
    val eq : $(map eq fields)
    val sqlp : $(map sql_injectable_prim fields)
    structure Debug : sig
        val show : $(map show fields)
        val label : $(map (fn _ => string) fields)
    end
end

functor Make(M : Input) = struct

open M

con fieldqs = map option fields

datatype message =
    Insert of $fields
  | Update of {Values : $fieldqs, Filter : $fieldqs}
  | Delete of $fieldqs

table tab : fields
table listeners : ([chan = channel message] ++ fieldqs)

val fl_listeners : folder ([chan = channel message] ++ fieldqs) =
    @Folder.cons [chan] [channel message] ! (@Folder.mp fl)

val sql = @mp [_] [_] @@sql_prim fl sqlp

val sql_listeners
    : $(map sql_injectable ([chan = channel message] ++ fieldqs)) =
    {chan = _} ++ @mp [_] [compose _ _] @@sql_option_prim fl sqlp

val selectListeners =
    @Sql.selectCompat ! ! ! fl sqlp _ listeners

fun insert xs =
    let
        val xqs = @Record.injqs ! fl Folder.nil xs
    in
        @Sql.insert fl sql tab xs;
        queryI1 (selectListeners xqs)
                (fn (row : {chan : _}) =>
                    debug "sending insert";
                    send row.chan (Insert xs))
    end

fun update [write] (sub : Subset.t fields write)
           (xs : $write) (filter : filter fields) =
    Subset.elim
        (fn [others] [write ~ others] fl_write _ (pf : Eq.t fields _) =>
            let
                val xqs = Subset.injqs xs
            in
                @Sql.update ! fl_write (Subset.projs sql)
                            (Eq.cast pf [fn fs => sql_table fs []] tab)
                            (Eq.cast pf [filter] filter).Sql
                            xs;
                queryI1 (selectListeners xqs)
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
        queryI1 (selectListeners xqs)
                (fn (row : {chan : _}) =>
                    debug "sending delete";
                    send row.chan (Delete xqs))
    end

con connection (read :: {Type}) =
    {Channel : channel message,
     Source : LinkedList.sources $fields}

fun connect [read] (q : query fields read) : transaction (connection read) =
    ch <- channel;
    @Sql.insert fl_listeners sql_listeners
                listeners ({chan = ch} ++ q.Fieldqs);
    ll <- LinkedList.mk (fn [t] => query1 (q.Sql tab));
    return {Channel = ch, Source = ll}

fun listen [read] (cxn : connection read) =
    let
        val ll = cxn.Source
        fun compat (xqs : $fieldqs) (ys : $fields) =
            @foldR3 [eq] [option] [ident] [fn _ => bool]
                    (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest]
                        (_ : eq t) xq y acc =>
                        case xq of
                            None => acc
                          | Some x => acc && x = y)
                    True
                    fl
                    eq xqs ys
        fun modify (xqs : $fieldqs) (ys : $fields) =
            @foldR2 [option] [ident] [record]
                    (fn [nm ::_] [t ::_] [rest ::_] [[nm] ~ rest]
                        xq y acc =>
                        case xq of
                            None => acc ++ {nm = y}
                          | Some x => acc ++ {nm = x})
                    {}
                    fl
                    xqs ys
        fun go msg =
            case msg of
                Insert xs => LinkedList.insert xs ll
              | Update u =>
                LinkedList.update (modify u.Values) (compat u.Filter) ll
              | Delete xqs => LinkedList.delete (compat xqs) ll
        val debugPrint =
            @LinkedList.Debug.print
                 (@Record.mkShow fl Debug.show Debug.label)
                 cxn.Source
    in
        spawnListener (fn msg => go msg; debugPrint) cxn.Channel
    end

fun value [read] (sub : Subset.t fields read) (cxn : connection read) =
    LinkedList.mp (@Subset.projs sub) (LinkedList.value cxn.Source)

val t =
    {Insert = insert,
     Update = @@update,
     Delete = delete,
     Connect =
      fn [read] sub q =>
         cxn <- @@connect [read] q;
         return {Listen = @@listen [read] cxn,
                 Value = @value sub cxn}}

structure Debug = struct
    val tab = tab
    con tab_hidden_constraints = []
    con empty :: {{Unit}} = []
    constraint tab_hidden_constraints ~ empty
end

end
