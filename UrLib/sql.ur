open Prelude

datatype insertResult = Inserted | NotInserted

val eq_insertResult : eq insertResult =
    mkEq (fn u v =>
             case (u, v) of
                 (Inserted, Inserted) => True
               | (NotInserted, NotInserted) => True
               | _ => False)

(* Only works on Postgres! *)
fun tryUnique dml =
    errq <- tryDml dml;
    case errq of
        None => return Inserted
      | Some err =>
        if String.isPrefix {Full = err,
                            Prefix = "ERROR:  duplicate key value"} then
            (* Here this means "not inserted" rather than "updated". *)
            return NotInserted
        else
            impossible (_LOC_ ^ ": " ^ err)

fun sqlInjectRow
        [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
        [fields ::: {Type}]
        (fl : folder fields) (sql : $(map sql_injectable fields))
        (xs : $fields) =
    @map2 [sql_injectable] [ident] [sql_exp tables agg exps]
          (@@sql_inject [tables] [agg] [exps]) fl sql xs

fun insert
        [fields ::: {Type}] [uniques ::: {{Unit}}]
        (fl : folder fields) (sql : $(map sql_injectable fields))
        (tab : sql_table fields uniques) (xs : $fields) =
    dml (Basis.insert tab (@sqlInjectRow fl sql xs))

fun update
        [unchanged ::: {Type}] [uniques ::: {{Unit}}]
        [changed ::: {Type}] [changed ~ unchanged]
        (fl : folder changed) (sql : $(map sql_injectable changed))
        (tab : sql_table (changed ++ unchanged) uniques)
        (cond : sql_exp [T = changed ++ unchanged] [] [] bool)
        (xs : $changed) =
    dml (Basis.update [changed] (@sqlInjectRow fl sql xs) tab cond)

fun delete
        [fields ::: {Type}] [uniques ::: {{Unit}}]
        (tab : sql_table fields uniques)
        (cond : sql_exp [T = fields] [] [] bool) =
    dml (Basis.delete tab cond)

fun select
        [vals ::: {Type}] [others ::: {Type}] [vals ~ others]
        [tabl] (_ : fieldsOf tabl (vals ++ others))
        (tab : tabl) (cond : sql_exp [T = vals ++ others] [] [] bool) =
    (SELECT T.{{vals}} FROM tab AS T WHERE {cond})

fun count
        [fields ::: {Type}] [tabl] (_ : fieldsOf tabl fields)
        (tab : tabl) (cond : sql_exp [T = fields] [] [] bool) =
    oneRowE1 (SELECT COUNT( * ) AS Count FROM tab AS T WHERE {cond})

fun exists
        [fields ::: {Type}] [tabl] (_ : fieldsOf tabl fields)
        (tab : tabl) (cond : sql_exp [T = fields] [] [] bool) =
    hasRows (SELECT TRUE FROM tab AS T WHERE {cond} LIMIT 1)

con lookupAcc
        (tabs :: {{Type}}) (agg :: {{Type}}) (exps :: {Type})
        (others :: {Type}) (tab :: Name) (r :: {Type}) =
    rest :: {Type} ->
    [[tab] ~ tabs] => [r ~ others] => [rest ~ r] => [rest ~ others]
    => sql_exp ([tab = r ++ rest ++ others] ++ tabs) agg exps bool

fun lookup
        [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
        [tab ::: Name] [keys ::: {Type}] [others ::: {Type}]
        [keys ~ others] [[tab] ~ tabs]
        (fl : folder keys) (sql : $(map sql_injectable keys))
        (ks : $keys)
    : sql_exp ([tab = keys ++ others] ++ tabs) agg exps bool =
    let
        fun equality [nm :: Name] [t :: Type]
                     [r :: {Type}] [[nm] ~ r]
                     (_ : sql_injectable t) (k : t)
                     (acc : lookupAcc tabs agg exps others tab r)
                     [rest :: {Type}]
                     [[tab] ~ tabs] [[nm = t] ++ r ~ others]
                     [rest ~ [nm = t] ++ r] [rest ~ others] =
            (SQL {{tab}}.{nm} = {[k]} AND {(acc [[nm = t] ++ rest])})
    in
        @foldR2 [sql_injectable] [ident]
                [lookupAcc tabs agg exps others tab]
                equality
                (fn [rest ::_] [_~_] [_~_] [_~_] [_~_] => (SQL TRUE))
                fl sql ks [[]] ! ! ! !
    end

fun lookups
        [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
        [tab ::: Name] [keys ::: {Type}] [others ::: {Type}]
        [keys ~ others] [[tab] ~ tabs]
        (fl : folder keys) (sql : $(map sql_injectable keys))
    : list $keys -> sql_exp ([tab = keys ++ others] ++ tabs) agg exps bool =
    List.foldl (fn ks acc => (SQL {acc} OR {@lookup ! ! fl sql ks}))
               (SQL FALSE)

fun selectLookup
        [keys ::: {Type}] [vals ::: {Type}] [others ::: {Type}]
        [keys ~ vals] [keys ~ others] [vals ~ others]
        (fl : folder keys) (sql : $(map sql_injectable keys))
        [tabl] (_ : fieldsOf tabl (keys ++ vals ++ others))
        (tab : tabl) (ks : $keys) =
    @@select [vals] [keys ++ others] ! [_] _ tab (@lookup ! ! fl sql ks)

fun countLookup
        [keys ::: {Type}] [others ::: {Type}] [keys ~ others]
        (fl : folder keys) (sql : $(map sql_injectable keys))
        [tabl] (_ : fieldsOf tabl (keys ++ others))
        (tab : tabl) (ks : $keys) =
    @@count [keys ++ others] [_] _ tab (@lookup ! ! fl sql ks)

fun existsLookup
        [keys ::: {Type}] [others ::: {Type}] [keys ~ others]
        (fl : folder keys) (sql : $(map sql_injectable keys))
        [tabl] (_ : fieldsOf tabl (keys ++ others))
        (tab : tabl) (ks : $keys) =
    @@exists [keys ++ others] [_] _ tab (@lookup ! ! fl sql ks)

fun updateLookup
        [keys ::: {Type}] [vals ::: {Type}] [others ::: {Type}]
        [uniques ::: {{Unit}}]
        [keys ~ vals] [keys ~ others] [vals ~ others]
        (fl_keys : folder keys) (sql_keys : $(map sql_injectable keys))
        (fl_vals : folder vals) (sql_vals : $(map sql_injectable vals))
        (tab : sql_table (keys ++ vals ++ others) uniques)
        (ks : $keys) (xs : $vals) =
    @@update [keys ++ others] [uniques] [vals] ! fl_vals sql_vals
             tab (@lookup ! ! fl_keys sql_keys ks) xs

fun insertLookup
        [keys ::: {Type}] [vals ::: {Type}]
        [nm ::: Name] [uniques ::: {{Unit}}]
        [keys ~ vals] [[nm] ~ uniques]
        (fl_keys : folder keys) (sql_keys : $(map sql_injectable keys))
        (fl_vals : folder vals) (sql_vals : $(map sql_injectable vals))
        (tab : sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques))
        (ks : $keys) (xs : $vals) =
    let
        val fl = @Folder.concat ! fl_keys fl_vals
        val sql = sql_keys ++ sql_vals
    in
        tryUnique (Basis.insert tab (@sqlInjectRow fl sql (ks ++ xs)))
    end

fun setLookup
        [keys ::: {Type}] [vals ::: {Type}]
        [nm ::: Name] [uniques ::: {{Unit}}]
        [keys ~ vals] [[nm] ~ uniques]
        (fl_keys : folder keys) (sql_keys : $(map sql_injectable keys))
        (fl_vals : folder vals) (sql_vals : $(map sql_injectable vals))
        (tab : sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques))
        (ks : $keys) (xs : $vals) =
    r <- @insertLookup ! ! fl_keys sql_keys fl_vals sql_vals tab ks xs;
    when (r = NotInserted)
         (@updateLookup ! ! ! fl_keys sql_keys fl_vals sql_vals tab ks xs);
    return r

fun deleteLookup
        [keys ::: {Type}] [others ::: {Type}] [uniques ::: {{Unit}}]
        [keys ~ others]
        (fl : folder keys) (sql : $(map sql_injectable keys))
        (tab : sql_table (keys ++ others) uniques) (ks : $keys) =
    delete tab (@lookup ! ! fl sql ks)

fun insertRandKeys
        [keys ::: {Type}] [vals ::: {Type}]
        [nm ::: Name] [uniques ::: {{Unit}}]
        [keys ~ vals] [[nm] ~ uniques]
        (fl_keys : folder keys) (rng_keys : $(map Random.t keys))
        (sql_keys : $(map sql_injectable keys))
        (fl_vals : folder vals) (sql_vals : $(map sql_injectable vals))
        (tab : sql_table (keys ++ vals) ([nm = map forget keys] ++ uniques))
        (xs : $vals)
    : transaction $keys =
    let
        val sql = sql_keys ++ sql_vals
        val fl = @Folder.concat ! fl_keys fl_vals
        fun go () =
            ks <- @Monad.mapR _ [Random.t] [ident]
                              (fn [nm ::_] [t ::_] => @Random.gen)
                              fl_keys
                              rng_keys;
            r <- tryUnique (Basis.insert tab
                                         (@sqlInjectRow fl sql (ks ++ xs)));
            case r of
                Inserted => return ks
              | NotInserted => go ()
    in
        go ()
    end

fun updateRandKeys
        [keys ::: {Type}] [others ::: {Type}]
        [nm ::: Name] [uniques ::: {{Unit}}]
        [keys ~ others] [[nm] ~ uniques]
        (fl : folder keys) (rng : $(map Random.t keys))
        (sql : $(map sql_injectable keys))
        (tab : sql_table (keys ++ others) ([nm = map forget keys] ++ uniques))
        (ksOld : $keys)
    : transaction $keys =
    let
        fun go () =
            ksNew <- @Monad.mapR _ [Random.t] [ident]
                                 (fn [nm ::_] [t ::_] => @Random.gen)
                                 fl
                                 rng;
            r <- tryUnique (Basis.update [keys]
                                         (@sqlInjectRow fl sql ksNew)
                                         tab
                                         (@lookup ! ! fl sql ksOld));
            case r of
                Inserted => return ksNew
              | NotInserted => go ()
    in
        go ()
    end

con compatAcc
        (tabs :: {{Type}}) (agg :: {{Type}}) (exps :: {Type})
        (others :: {Type}) (tab :: Name) (r :: {Type}) =
    rest :: {Type} ->
    [[tab] ~ tabs] => [r ~ others] => [rest ~ r] => [rest ~ others]
    => sql_exp ([tab = map option (r ++ rest) ++ others] ++ tabs) agg exps bool

fun compat
        [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
        [tab ::: Name] [keys ::: {Type}] [others ::: {Type}]
        [keys ~ others] [[tab] ~ tabs]
        (fl : folder keys) (sqlp : $(map sql_injectable_prim keys))
        (kqs : $(map option keys))
    : sql_exp ([tab = (map option keys) ++ others] ++ tabs) agg exps bool =
    let
        fun equality [nm :: Name] [t :: Type]
                     [r :: {Type}] [[nm] ~ r]
                     (_ : sql_injectable_prim t) (kq : option t)
                     (acc : compatAcc tabs agg exps others tab r)
                     [rest :: {Type}]
                     [[tab] ~ tabs] [[nm = t] ++ r ~ others]
                     [rest ~ [nm = t] ++ r] [rest ~ others] =
            let
                val accNew = acc [[nm = t] ++ rest]
            in
                case kq of
                    None => accNew
                  | Some _ =>
                    (SQL ({{tab}}.{nm} IS NULL OR {{tab}}.{nm} = {[kq]})
                     AND {accNew})
            end
    in
        @foldR2 [sql_injectable_prim] [option]
                [compatAcc tabs agg exps others tab]
                equality
                (fn [rest ::_] [_~_] [_~_] [_~_] [_~_] => (SQL TRUE))
                fl sqlp kqs [[]] ! ! ! !
    end

fun selectCompat
        [keys ::: {Type}] [vals ::: {Type}] [others ::: {Type}]
        [keys ~ vals] [keys ~ others] [vals ~ others]
        (fl : folder keys) (sqlp : $(map sql_injectable_prim keys))
        [tabl] (_ : fieldsOf tabl (map option keys ++ vals ++ others))
        (tab : tabl) (kqs : $(map option keys)) =
    @@select [vals] [map option keys ++ others] ! [_] _
             tab (@compat ! ! fl sqlp kqs)
