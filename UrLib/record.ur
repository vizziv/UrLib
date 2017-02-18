fun proj [nm ::_] [t] [drop] [[nm] ~ drop] (xs : $([nm = t] ++ drop)) = xs.nm

val proj1 = fn [nm] [t] => proj [nm]

fun projs [keep] [drop] [keep ~ drop] (xs : $(keep ++ drop)) = xs --- drop

fun rename [nm1 ::_] [nm2 ::_] [t] [ts] [[nm1] ~ ts] [[nm2] ~ ts]
           (xs : $([nm1 = t] ++ ts)) =
    xs -- nm1 ++ {nm2 = xs.nm1}

fun curry [have] [need] [t] [have ~ need]
          (f : $(have ++ need) -> t) (xs : $have) (ys : $need) =
    f (xs ++ ys)

fun snoc [ts] (xs : $ts) [nm :: Name] [t] [[nm] ~ ts] (x : t) = xs ++ {nm = x}

fun set [keep] [drop] [insert] [keep ~ drop] [keep ~ insert]
        (xs : $(keep ++ drop)) (ys : $(insert))
        : $(keep ++ insert) =
    xs --- drop ++ ys

fun injqs [keep] [drop] [keep ~ drop]
          (fl_keep : folder keep) (fl_drop : folder drop)
          (xs : $keep) =
    @mp [ident] [option] @@Some fl_keep xs
    ++ @map0 [option] (fn [t ::_] => None) fl_drop

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
