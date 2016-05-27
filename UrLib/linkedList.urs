include Prelude.Types

con signals :: Type -> Type
val mp : a ::: Type -> b ::: Type -> (a -> b) -> signals a -> signals b
val foldl : a ::: Type -> b ::: Type
            -> (a -> b -> b) -> b -> signals a -> signal b
val mapX : a ::: Type -> ctx ::: {Unit} -> [[Dyn] ~ ctx]
           => (a -> xml ([Dyn] ++ ctx) [] [])
           -> signals a -> xml ([Dyn] ++ ctx) [] []

con sources :: Type -> Type
(* Argument should call the [a -> tunit] for each [a] to put in the list. *)
val mk : a ::: Type -> ((a -> tunit) -> tunit) -> transaction (sources a)
val value : a ::: Type -> sources a -> signals a
val insert : a ::: Type -> a -> sources a -> tunit
val update : a ::: Type -> (a -> a) -> (a -> bool) -> sources a -> tunit
val delete : a ::: Type -> (a -> bool) -> sources a -> tunit
