open Prelude

con cont fields keep res =
    drop ::: {Type} -> [keep ~ drop]
    => folder keep -> folder drop -> Eq.t fields (keep ++ drop)
    -> res

con t fields keep = res ::: Type -> cont fields keep res -> res

fun intro [keep] [drop] [keep ~ drop] flKeep flDrop
          [res] (k : cont (keep ++ drop) keep res) =
    @@k [drop] ! flKeep flDrop Eq.refl

fun mp [fields] [keep] [f] (t : t fields keep)
       [res] (k : cont (map f fields) (map f keep) res) =
    t (fn [drop] [keep ~ drop]
          flKeep flDrop (pf : Eq.t fields (keep ++ drop)) =>
          @@k [map f drop] !
              (@Folder.mp flKeep) (@Folder.mp flDrop)
              (Eq.cast pf [compose (Eq.t (map f fields)) (map f)] Eq.refl))

fun elim [fields] [keep] t = @@t

fun projs [fields] [keep] (t : t fields keep) =
    t (fn [drop] [keep ~ drop] _ _ pf xs => Eq.cast pf [record] xs --- drop)
