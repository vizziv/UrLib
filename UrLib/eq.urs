con t :: K --> K -> K -> Type

val refl : K --> a ::: K -> t a a

val symm : K --> a ::: K -> b ::: K -> t a b -> t b a

val trans :
    K --> a ::: K -> b ::: K -> c ::: K -> t a b -> t b c -> t a c

val cast :
    K -->
    a ::: K -> b ::: K ->
    t a b -> tf :: (K -> Type) ->
    tf a
    -> tf b

val over :
    K -->
    a ::: K -> b ::: K ->
    t a b -> tf :: (K -> Type) ->
    (tf b -> tf b)
    -> tf a -> tf a

val mp :
    K1 --> K2 -->
    a ::: K1 -> b ::: K1 -> tf :: (K1 -> K2) ->
    t a b
    -> t (tf a) (tf b)

val make :
    nm :: Name -> a ::: Type -> others ::: {Type} -> r ::: {Type} ->
    [[nm] ~ others] =>
    t ([nm = a] ++ others) r ->
    a
    -> variant r

val makeMap :
    K -->
    nm :: Name -> a ::: K -> others ::: {K} -> r ::: {K} ->
    [[nm] ~ others] =>
    tf :: (K -> Type) ->
    t ([nm = a] ++ others) r ->
    tf a
    -> variant (map tf r)
