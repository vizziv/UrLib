include Prelude.Types

structure Signal : sig
    con t :: Type -> Type
    val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b
    val foldl : a ::: Type -> b ::: Type
                -> (a -> b -> b) -> b -> t a -> signal b
    val mapX : a ::: Type -> ctx ::: {Unit} -> [[Dyn] ~ ctx]
               => (a -> xml ([Dyn] ++ ctx) [] [])
               -> t a -> xml ([Dyn] ++ ctx) [] []
end

structure Source : sig
    con t :: Type -> Type
    (* Argument is a fold, such as a query. *)
    val mk : a ::: Type -> ((a -> tunit) -> tunit) -> transaction (t a)
    val value : a ::: Type -> t a -> Signal.t a
    val insert : a ::: Type -> a -> t a -> tunit
    val update : a ::: Type -> (a -> a) -> (a -> bool) -> t a -> tunit
    val delete : a ::: Type -> (a -> bool) -> t a -> tunit
end
