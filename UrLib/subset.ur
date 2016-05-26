con cont fields keep res =
    drop ::: {Type} -> [keep ~ drop]
    => folder keep -> folder drop -> Eq.t fields (keep ++ drop)
    -> res

con t fields keep = res ::: Type -> cont fields keep res -> res

fun intro [keep] [drop] [keep ~ drop] flKeep flDrop [res]
          (k : cont (keep ++ drop) keep res) =
    @@k [drop] ! flKeep flDrop Eq.refl

fun elim [fields] [keep] t = @@t

fun projs [fields] [keep] (f : t fields keep) =
    f (fn [drop] [keep ~ drop] _ _ pf xs => Eq.cast pf [record] xs --- drop)
