type tunit = transaction unit

val id : t ::: Type -> t -> t

val maximum : t ::: Type -> ord t -> t -> list t -> t
val minimum : t ::: Type -> ord t -> t -> list t -> t

val cases : ts ::: {Type} -> u ::: Type
            -> $(map (fn t => t -> u) ts) -> variant ts -> u

val sub : keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop]
          => $(keep ++ drop) -> $keep

val spawnListener : t ::: Type -> (t -> tunit) -> channel t -> tunit

val mapNm : K --> tf1 :: (K -> Type) -> tf2 :: ({K} -> K -> Type)
            -> (done :: {K} -> todo :: {K}
                -> nm :: Name -> t ::: K
                -> [[nm] ~ done] => [done ++ [nm = t] ~ todo]
                => tf1 t -> tf2 (done ++ [nm = t] ++ todo) t)
            -> r ::: {K} -> folder r -> $(map tf1 r) -> $(map (tf2 r) r)
