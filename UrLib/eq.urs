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
