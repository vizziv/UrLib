con t :: K --> K -> K -> Type
val refl : K --> a ::: K -> t a a
val symm : K --> a ::: K -> b ::: K -> t a b -> t b a
val trans : K --> a ::: K -> b ::: K -> c ::: K
            -> t a b -> t b c -> t a c
val cast : K --> a ::: K -> b ::: K -> t a b -> tf :: (K -> Type)
           -> tf a -> tf b

(* Kind-polymorphic class instances can't be inferred, so we monomorphize the
   two most common cases. *)

class t0 :: Type -> Type -> Type
val mk0 : a ::: Type -> b ::: Type -> t a b -> t0 a b
val refl0 : a ::: Type -> t0 a a
val symm0 : a ::: Type -> b ::: Type -> t0 a b -> t0 b a
val trans0 : a ::: Type -> b ::: Type -> c ::: Type
             -> t0 a b -> t0 b c -> t0 a c
val cast0 : a ::: Type -> b ::: Type -> t0 a b -> tf :: (Type -> Type)
            -> tf a -> tf b

class t1 :: {Type} -> {Type} -> Type
val mk1 : a ::: {Type} -> b ::: {Type} -> t a b -> t1 a b
val refl1 : a ::: {Type} -> t1 a a
val symm1 : a ::: {Type} -> b ::: {Type} -> t1 a b -> t1 b a
val trans1 : a ::: {Type} -> b ::: {Type} -> c ::: {Type}
             -> t1 a b -> t1 b c -> t1 a c
val cast1 : a ::: {Type} -> b ::: {Type} -> t1 a b -> tf :: ({Type} -> Type)
            -> tf a -> tf b
