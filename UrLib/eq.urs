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
    K1 --> K2 -->
    a1 ::: K1 -> b1 ::: K1 -> a2 ::: K2 -> b2 ::: K2 ->
    t a1 b1 -> tf1 :: (K1 -> Type) -> t a2 b2 -> tf2 :: (K2 -> Type) ->
    (tf1 b1 -> tf2 b2)
    -> tf1 a1 -> tf2 a2

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
