fun set [const] [modify] [insert]
        [const ~ modify] [const ~ insert] [modify ~ insert]
        (xs : $(const ++ modify)) (ys : $(modify ++ insert))
        : $(const ++ modify ++ insert) =
    xs --- modify ++ ys

fun mkShow [ts] (fl : folder ts) (shows : $(map show ts))
           (labels : $(map (fn _ => string) ts)) =
    Basis.mkShow
        (fn (xs : $ts) =>
            "{"
            ^ @foldR3 [show] [fn _ => string] [ident] [fn _ => string]
                      (fn [nm ::_] [t ::_] [rest ::_] [_~_]
                          (_ : show t) label x acc =>
                          acc ^ label ^ " = " ^ show x ^ ", ")
                      ""
                      fl
                      shows labels xs
            ^ "}")
