include Prelude.Types

(* Read-only list of signals. *)
con signals :: Type -> Type

(* Map a function over a list. *)
val mp : a ::: Type -> b ::: Type -> (a -> b) -> signals a -> signals b

(* [Mappable.t] instance. *)
val mappable_signals : Mappable.t signals

(* Left fold. *)
val foldl :
    a ::: Type -> b ::: Type ->
    (a -> b -> b) -> b ->
    signals a
    -> signal b

(* Generate dynamic XML for each item in a list. *)
val mapX :
    a ::: Type -> ctx ::: {Unit} -> [[Dyn] ~ ctx] =>
    (a -> xml ([Dyn] ++ ctx) [] []) ->
    signals a
    -> xml ([Dyn] ++ ctx) [] []

(* Write-only list of sources. *)
con sources :: Type -> Type

(* Build a list of sources in fold-like style.
   The [a -> b -> transaction b] is "snoc" and the [b] is "nil".
 *)
val mk :
    a ::: Type ->
    (b ::: Type -> (a -> b -> transaction b) -> b -> transaction b)
    -> transaction (sources a)

(* Get a read-only view of the list. *)
val value : a ::: Type -> sources a -> signals a

(* Append a new item to the end of the list. *)
val insert : a ::: Type -> a -> sources a -> tunit

(* Apply a function to all items matching a predicate. *)
val update : a ::: Type -> (a -> a) -> (a -> bool) -> sources a -> tunit

(* Delete all items matching a predicate. *)
val delete : a ::: Type -> (a -> bool) -> sources a -> tunit

structure Debug : sig
    val print : a ::: Type -> show a -> sources a -> tunit
end
