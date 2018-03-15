include Prelude.Types

(* Read-only list of signals. *)
con signals :: Type -> Type

val mp : a ::: Type -> b ::: Type -> (a -> b) -> signals a -> signals b

val foldl :
    a ::: Type -> b ::: Type ->
    (a -> b -> b) -> b ->
    signals a
    -> signal b

val mapX :
    a ::: Type -> ctx ::: {Unit} -> [[Dyn] ~ ctx] =>
    (a -> xml ([Dyn] ++ ctx) [] []) ->
    signals a
    -> xml ([Dyn] ++ ctx) [] []

(* Write-only list of sources. *)
con sources :: Type -> Type

(* Argument should be a fold that builds the list.
   It should use the [a -> b -> transaction b] function as "snoc". *)
val mk :
    a ::: Type ->
    (b ::: Type -> (a -> b -> transaction b) -> b -> transaction b)
    -> transaction (sources a)

val value : a ::: Type -> sources a -> signals a

val insert : a ::: Type -> a -> sources a -> tunit

val update : a ::: Type -> (a -> a) -> (a -> bool) -> sources a -> tunit

val delete : a ::: Type -> (a -> bool) -> sources a -> tunit

structure Debug : sig
    val print : a ::: Type -> show a -> sources a -> tunit
end
