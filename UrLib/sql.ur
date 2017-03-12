open Prelude

fun sqlInjectRow [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
                 [fields ::: {Type}]
                 (fl : folder fields) (sql : $(map sql_injectable fields))
                 (xs : $fields) =
    @map2 [sql_injectable] [ident] [sql_exp tables agg exps]
          (@@sql_inject [tables] [agg] [exps]) fl sql xs

fun insert [fields ::: {Type}] [uniques ::: {{Unit}}]
           (fl : folder fields) (sql : $(map sql_injectable fields))
           (tab : sql_table fields uniques) (xs : $fields) =
    dml (Basis.insert tab (@sqlInjectRow fl sql xs))

fun update [unchanged ::: {Type}] [uniques ::: {{Unit}}]
           [changed ::: {Type}] [changed ~ unchanged]
           (fl : folder changed) (sql : $(map sql_injectable changed))
           (tab : sql_table (changed ++ unchanged) uniques)
           (cond : sql_exp [T = changed ++ unchanged] [] [] bool)
           (xs : $changed) =
    dml (Basis.update [changed] (@sqlInjectRow fl sql xs) tab cond)

fun delete [fields ::: {Type}] [uniques ::: {{Unit}}]
           (tab : sql_table fields uniques)
           (cond : sql_exp [T = fields] [] [] bool) =
    dml (Basis.delete tab cond)

fun select [vals ::: {Type}] [others ::: {Type}] [vals ~ others]
           [tabl] (_ : fieldsOf tabl (vals ++ others))
           (tab : tabl) (cond : sql_exp [T = vals ++ others] [] [] bool) =
    let
        val q1 =
            sql_query1
                [[]]
                {Distinct = False,
                 From = sql_from_table [#T] tab,
                 Where = cond,
                 GroupBy = sql_subset_all [[T = vals ++ others]],
                 Having = sql_inject True,
                 SelectFields = sql_subset [[T = (vals, others)]],
                 SelectExps = {}}
    in
        sql_query
            {Rows = q1,
             OrderBy = sql_order_by_Nil [[]],
             Limit = sql_no_limit,
             Offset = sql_no_offset}
    end

con lookupAcc (tabs :: {{Type}}) (agg :: {{Type}}) (exps :: {Type})
              (others :: {Type}) (tab :: Name) (r :: {Type}) =
    rest :: {Type}
    -> [[tab] ~ tabs] => [r ~ others] => [rest ~ r] => [rest ~ others]
    => sql_exp ([tab = r ++ rest ++ others] ++ tabs) agg exps bool

fun lookup [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
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

fun lookups [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
            [tab ::: Name] [keys ::: {Type}] [others ::: {Type}]
            [keys ~ others] [[tab] ~ tabs]
            (fl : folder keys) (sql : $(map sql_injectable keys))
    : list $keys -> sql_exp ([tab = keys ++ others] ++ tabs) agg exps bool =
    List.foldl (fn ks acc => (SQL {acc} OR {@lookup ! ! fl sql ks}))
               (SQL FALSE)

fun selectLookup [keys ::: {Type}] [vals ::: {Type}] [others ::: {Type}]
                 [keys ~ vals] [keys ~ others] [vals ~ others]
                 (fl : folder keys) (sql : $(map sql_injectable keys))
                 [tabl] (_ : fieldsOf tabl (keys ++ vals ++ others))
                 (tab : tabl) (ks : $keys) =
    @@select [vals] [keys ++ others] ! [_] _ tab (@lookup ! ! fl sql ks)

fun selectLookups [keys ::: {Type}] [vals ::: {Type}] [others ::: {Type}]
                  [keys ~ vals] [keys ~ others] [vals ~ others]
                  (fl : folder keys) (sql : $(map sql_injectable keys))
                  [tabl] (_ : fieldsOf tabl (keys ++ vals ++ others))
                  (tab : tabl) (kss : list $keys) =
    @@select [vals] [keys ++ others] ! [_] _ tab (@lookups ! ! fl sql kss)

fun updateLookup [unchanged ::: {Type}] [uniques ::: {{Unit}}]
                 [changed ::: {Type}] [changed ~ unchanged]
                 (fl_changed : folder changed)
                 (sql_changed : $(map sql_injectable changed))
                 [keys ::: {Type}] [keys ~ changed ++ unchanged]
                 (fl_keys : folder keys)
                 (sql_keys : $(map sql_injectable keys))
                 (tab : sql_table (keys ++ changed ++ unchanged) uniques)
                 (ks : $keys) (xs : $changed) =
    @@update [keys ++ unchanged] [uniques] [changed] ! fl_changed sql_changed
             tab (@lookup ! ! fl_keys sql_keys ks) xs

fun deleteLookup [keys ::: {Type}] [others ::: {Type}] [uniques ::: {{Unit}}]
                 [keys ~ others]
                 (fl : folder keys) (sql : $(map sql_injectable keys))
                 (tab : sql_table (keys ++ others) uniques) (ks : $keys) =
    delete tab (@lookup ! ! fl sql ks)

con compatAcc (tabs :: {{Type}}) (agg :: {{Type}}) (exps :: {Type})
              (others :: {Type}) (tab :: Name) (r :: {Type}) =
    rest :: {Type}
    -> [[tab] ~ tabs] => [r ~ others] => [rest ~ r] => [rest ~ others]
    => sql_exp ([tab = map option (r ++ rest) ++ others] ++ tabs) agg exps bool

fun compat [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
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

fun selectCompat [keys ::: {Type}] [vals ::: {Type}] [others ::: {Type}]
                 [keys ~ vals] [keys ~ others] [vals ~ others]
                 (fl : folder keys) (sqlp : $(map sql_injectable_prim keys))
                 [tabl] (_ : fieldsOf tabl (map option keys ++ vals ++ others))
                 (tab : tabl) (kqs : $(map option keys)) =
    @@select [vals] [map option keys ++ others] ! [_] _
             tab (@compat ! ! fl sqlp kqs)
