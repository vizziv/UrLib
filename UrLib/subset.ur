con cont fields keep res =
    drop ::: {Type} -> [keep ~ drop] => Eq.t fields (keep ++ drop) -> res

con t fields keep = res ::: Type -> cont fields keep res -> res

fun mk [keep] [drop] [keep ~ drop] [res] (f : cont (keep ++ drop) keep res) =
    @@f [drop] ! Eq.refl

fun projs [fields] [keep] (f : t fields keep) =
    f (fn [drop] [keep ~ drop] pf xs => Eq.cast pf [record] xs --- drop)
