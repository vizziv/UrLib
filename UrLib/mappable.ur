open Prelude

con t (f :: Type -> Type) = a ::: Type -> b ::: Type -> (a -> b) -> f a -> f b

val mk = fn [f] => identity

val mp = fn [f] => identity

val monad = @@Monad.mp

val list = @@List.mp

fun field
        [nm ::_] [ts] [[nm] ~ ts]
        [a] [b]
        (f : a -> b) (r : $([nm = a] ++ ts)) =
    r -- nm ++ {nm = f r.nm}

fun choice
        [nm ::_] [ts] [[nm] ~ ts]
        (fl : folder ts)
        [a] [b]
        (f : a -> b)
        (x : variant ([nm = a] ++ ts))
    : variant ([nm = b] ++ ts) =
    match x
            ((@mapNm0 [fn t => t -> variant ([nm = b] ++ ts)]
                      (@Folder.cons [nm] [b] ! fl)
                      (fn [others ::_] [nm' ::_] [t] [[nm'] ~ others] _ pf =>
                          Eq.make [nm'] pf))
             -- nm ++ {nm = fn x => make [nm] (f x)})

fun compose [f] [g] (mpf : t f) (mpg : t g) [a] [b] = mpf <<< mpg
