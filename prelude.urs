type tunit = transaction unit

val id : t ::: Type -> t -> t

val maximum : t ::: Type -> ord t -> t -> list t -> t
val minimum : t ::: Type -> ord t -> t -> list t -> t

val distinct : t ::: Type -> eq t -> ord t -> list t -> bool

val cases : ts ::: {Type} -> u ::: Type
            -> $(map (fn t => t -> u) ts) -> variant ts -> u

val sub : keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop]
          => $(keep ++ drop) -> $keep

val curry : have ::: {Type} -> need ::: {Type} -> t ::: Type -> [have ~ need]
            => ($(have ++ need) -> t) -> $have -> $need -> t

val snoc : ts ::: {Type} -> nm :: Name -> t ::: Type -> [[nm] ~ ts]
           => t -> $ts -> $([nm = t] ++ ts)

val spawnListener : t ::: Type -> (t -> tunit) -> channel t -> tunit

val mapiPartial : a ::: Type -> b ::: Type
                  -> (int -> a -> option b) -> list a -> list b

val mapNm0 : K --> tf :: ({K} -> K -> Type)
             -> (others :: {K} -> nm :: Name -> t ::: K
                 -> [[nm] ~ others] => folder others
                 -> tf ([nm = t] ++ others) t)
             -> r ::: {K} -> folder r
             -> $(map (tf r) r)

val mapNm : K --> tf1 :: (K -> Type) -> tf2 :: ({K} -> K -> Type)
            -> (others :: {K} -> nm :: Name -> t ::: K
                -> [[nm] ~ others] => folder others
                -> tf1 t -> tf2 ([nm = t] ++ others) t)
            -> r ::: {K} -> folder r
            -> $(map tf1 r) -> $(map (tf2 r) r)
