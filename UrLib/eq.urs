(* Equality type: [t a b] means [a] equals [b]. *)
con t :: K --> K -> K -> Type

(* Reflexive property. *)
val refl : K --> a ::: K -> t a a

(* Symmetric property. *)
val symm : K --> a ::: K -> b ::: K -> t a b -> t b a

(* Transitive property. *)
val trans :
    K --> a ::: K -> b ::: K -> c ::: K -> t a b -> t b c -> t a c

(* Cast between two equal types. *)
val cast :
    K -->
    a ::: K -> b ::: K ->
    t a b -> tf :: (K -> Type) ->
    tf a
    -> tf b

(* Cast the domain and range of a function. *)
val over :
    K1 --> K2 -->
    a1 ::: K1 -> b1 ::: K1 -> a2 ::: K2 -> b2 ::: K2 ->
    t a1 b1 -> tf1 :: (K1 -> Type) -> t a2 b2 -> tf2 :: (K2 -> Type) ->
    (tf1 b1 -> tf2 b2)
    -> tf1 a1 -> tf2 a2

(* Equality is preserved by functions. *)
val mp :
    K1 --> K2 -->
    a ::: K1 -> b ::: K1 -> tf :: (K1 -> K2) ->
    t a b
    -> t (tf a) (tf b)

(* Convenient for constructing [variant]s, e.g. with [Prelude.mapNm]. *)
val make :
    nm :: Name -> a ::: Type -> others ::: {Type} -> r ::: {Type} ->
    [[nm] ~ others] =>
    t ([nm = a] ++ others) r ->
    a
    -> variant r

(* [make] generalized to work on kinds other than [Type]. *)
val makeMap :
    K -->
    nm :: Name -> a ::: K -> others ::: {K} -> r ::: {K} ->
    [[nm] ~ others] =>
    tf :: (K -> Type) ->
    t ([nm = a] ++ others) r ->
    tf a
    -> variant (map tf r)
