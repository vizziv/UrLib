con t :: Type -> Type

val mp : a ::: Type -> b ::: Type -> (a -> b) -> t a -> t b

val foldl : a ::: Type -> b ::: Type -> (a -> b -> b) -> b -> t a -> signal b

val mapX : a ::: Type -> ctx ::: {Unit} -> [[Dyn] ~ ctx]
           => (a -> xml ([Dyn] ++ ctx) [] [])
           -> t a -> xml ([Dyn] ++ ctx) [] []
