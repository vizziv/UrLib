open Prelude

con cont fields keep res =
    drop ::: {Type} -> [keep ~ drop] =>
    folder keep -> folder drop -> Eq.t fields (keep ++ drop)
    -> res

con t fields keep = res ::: Type -> cont fields keep res -> res

fun intro
        [keep] [drop] [keep ~ drop] fl_keep fl_drop
        [res] (k : cont (keep ++ drop) keep res) =
    @@k [drop] ! fl_keep fl_drop Eq.refl

fun mp
        [fields] [keep] [f] (t : t fields keep)
        [res] (k : cont (map f fields) (map f keep) res) =
    t (fn [drop] [keep ~ drop]
          fl_keep fl_drop (pf : Eq.t fields (keep ++ drop)) =>
          @@k [map f drop] !
              (@Folder.mp fl_keep) (@Folder.mp fl_drop)
              (Eq.cast pf [compose (Eq.t (map f fields)) (map f)] Eq.refl))

fun elim [fields] [keep] t = @@t

fun fl [fields] [keep] (t : t fields keep) =
    t (fn [drop] [keep ~ drop] fl_keep _ _ => fl_keep)

fun projs [fields] [keep] (t : t fields keep) =
    t (fn [drop] [keep ~ drop] _ _ pf (xs : $fields) =>
          Eq.cast pf [record] xs --- drop)

fun over [fields] [keep] (t : t fields keep) =
    t (fn [drop] [keep ~ drop] _ _ pf (f : $keep -> $keep) =>
          Eq.over pf [record] (fn xs => xs --- keep ++ f (xs --- drop)))

fun injqs [fields] [keep] (t : t fields keep) =
    t (fn [drop] [keep ~ drop]
          fl_keep fl_drop (pf : Eq.t fields (keep ++ drop))
          (xs : $keep) =>
          Eq.cast (Eq.symm pf) [compose record (map _)]
                  (@Record.injqs ! fl_keep fl_drop xs))
