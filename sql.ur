fun sqlInjectRow [tables ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
                 [ts ::: {Type}] (f : folder ts)
                 (injs : $(map sql_injectable ts)) (xs : $ts) =
    @map2 [sql_injectable] [ident] [sql_exp tables agg exps]
          (@@sql_inject [tables] [agg] [exps]) f injs xs

fun insertRow [fields ::: {Type}] [uniques ::: {{Unit}}]
              (f : folder fields) (injs : $(map sql_injectable fields))
              (tab : sql_table fields uniques) (row : $fields) =
    dml (insert tab (@sqlInjectRow f injs row))

con whereEqAcc (tabs :: {{Type}}) (agg :: {{Type}}) (exps :: {Type})
               (others :: {Type}) (tab :: Name) (r :: {Type}) =
    rest :: {Type}
    -> [[tab] ~ tabs] => [r ~ others] => [rest ~ r] => [rest ~ others]
    => sql_exp ([tab = r ++ rest ++ others] ++ tabs) agg exps bool

fun whereEq [tabs ::: {{Type}}] [agg ::: {{Type}}] [exps ::: {Type}]
            [keys ::: {Type}] [others ::: {Type}]
            [tab :: Name]
            [keys ~ others] [[tab] ~ tabs] (fl : folder keys)
            (injs : $(map sql_injectable keys)) (ks : $keys)
    : sql_exp ([tab = keys ++ others] ++ tabs) agg exps bool =
    let
        fun equality [nm :: Name] [t :: Type]
                     [r :: {Type}] [[nm] ~ r]
                     (_ : sql_injectable t) (k : t)
                     (acc : whereEqAcc tabs agg exps others tab r)
                     [rest :: {Type}]
                     [[tab] ~ tabs] [[nm = t] ++ r ~ others]
                     [rest ~ [nm = t] ++ r] [rest ~ others] =
            (SQL {{tab}}.{nm} = {[k]} AND {(acc [[nm = t] ++ rest])})
    in
        @foldR2 [sql_injectable] [ident]
                [whereEqAcc tabs agg exps others tab]
                equality
                (fn [rest ::_] [_~_] [_~_] [_~_] [_~_] => (SQL TRUE))
                fl injs ks [[]] ! ! ! !
    end

fun selectWhereEq [keys ::: {Type}] [vals ::: {Type}] [other ::: {Type}]
                  [keys ~ vals] [keys ~ other] [vals ~ other]
                  (fl : folder keys) (injs : $(map sql_injectable keys))
                  [tabl] (_ : fieldsOf tabl (keys ++ vals ++ other))
                  (tab : tabl) (ks : $keys) : sql_query [] [] _ _ =
    let
        val q1 =
            sql_query1
                [[]]
                {Distinct = False,
                 From = sql_from_table [#T] tab,
                 Where = @whereEq [#T] ! ! fl injs ks,
                 GroupBy = sql_subset [[T = (vals, keys ++ other)]],
                 Having = sql_inject True,
                 SelectFields = sql_subset_all [[T = vals]],
                 SelectExps = {}}
    in
        sql_query
            {Rows = q1,
             OrderBy = sql_order_by_Nil [[]],
             Limit = sql_no_limit,
             Offset = sql_no_offset}
    end
