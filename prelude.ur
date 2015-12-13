type tunit = transaction unit

fun id [t] (x : t) = x

fun maximum [t] (_ : ord t) : t -> list t -> t = List.foldl max
fun minimum [t] (_ : ord t) : t -> list t -> t = List.foldl min

fun cases [ts ::: {Type}] [u] (fs : $(map (fn t => t -> u) ts)) v = match v fs

fun sub [keep] [drop] [keep ~ drop] (xs : $(keep ++ drop)) = xs --- drop

fun spawnListener [t] (action : t -> transaction unit) (chan : channel t) =
    let
        fun listen () = x <- recv chan; action x; listen ()
    in
        spawn (listen ())
    end

fun mapNm [K] [tf1 :: K -> Type]
          [tf2 :: {K} -> K -> Type]
          (f : done :: {K} -> todo :: {K}
               -> nm :: Name -> t ::: K
               -> [[nm] ~ done] => [done ++ [nm = t] ~ todo]
               => tf1 t
               -> tf2 (done ++ [nm = t] ++ todo) t)
          [r ::: {K}] (fl : folder r)
    : $(map tf1 r) -> $(map (tf2 r) r) =
    @@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo]
               => $(map tf1 done) -> $(map (tf2 (done ++ todo)) done)]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo]
                      => $(map tf1 done) -> $(map (tf2 (done ++ todo)) done))
               [todo :: {K}] [done ++ [nm = t] ~ todo]
               (x : $(map tf1 (done ++ [nm = t]))) =>
               acc [[nm = t] ++ todo] (x -- nm)
                   ++ {nm = f [done] [todo] [nm] x.nm})
           (fn [todo :: {K}] [[] ~ todo] {} => {})
           [r] fl [[]] !
