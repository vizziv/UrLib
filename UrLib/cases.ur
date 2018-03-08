open Prelude

fun get [K] [r ::: {K}] (fl : folder r) [t ::: Type] =
    @@cases [map (fn _ => t) r] [t]
            (@map0 [fn _ => t -> t] (fn [ignore ::_] => ident) fl)

fun mappable
        [r ::: {Type}] (fl : folder r)
        [f ::: Type -> Type] (_ : Mappable.t f) =
    @@cases [map f r] [f (variant r)]
            (@mapNm0 [fn t => f t -> f (variant r)] fl
                     (fn [others ::_] [nm ::_] [t] [[nm] ~ others ] _ pf =>
                         Mappable.mp (Eq.make [nm] pf)))

fun mp
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (fs : $(map (fn t :: K => tf1 t -> tf2 t) r))
    : variant (map tf1 r)
      -> variant (map tf2 r) =
    @@cases [map tf1 r] [_]
            (@mapNm [fn t => tf1 t -> tf2 t]
                    [fn t => tf1 t -> variant (map tf2 r)]
                    fl
                    (fn [others ::_] [nm ::_] [t] [[nm] ~ others] _ pf =>
                        compose (Eq.makeMap [nm] [tf2] pf))
                    fs)

fun mapU
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (f : t ::: K -> tf1 t -> tf2 t)
    : variant (map tf1 r)
      -> variant (map tf2 r) =
    @mp [tf1] [tf2] fl
        (@map0 [fn t => tf1 t -> tf2 t] (fn [t ::_] => f) fl)

fun traverse
    [K]
    [tf1 :: K -> Type] [tf2 :: K -> Type]
    [r ::: {K}] (fl : folder r)
    [f ::: Type -> Type] (_ : Mappable.t f)
    (fs : $(map (fn t :: K => tf1 t -> f (tf2 t)) r))
    : variant (map tf1 r) -> f (variant (map tf2 r)) =
    @@cases [map tf1 r] [_]
            (@mapNm [fn t => tf1 t -> f (tf2 t)]
                    [fn t => tf1 t -> f (variant (map tf2 r))]
                    fl
                    (fn [others ::_] [nm ::_] [t]
                        [[nm] ~ others] _ pf =>
                        compose (Mappable.mp (Eq.makeMap [nm] [tf2] pf)))
                    fs)

fun diagTraverse
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        [f ::: Type -> Type] (_ : monad f)
        (fs : $(map (fn t :: K => tf1 t -> tf2 t -> f (tf3 t)) r))
    : variant (map tf1 r) ->
      variant (map tf2 r)
      -> f (option (variant (map tf3 r))) =
    let
        fun nones
                [others] [nm] [t]
                [[nm] ~ others] (fl_others : folder others) =
            @map0 [fn u =>
                      tf2 u ->
                      f (option (variant (map tf3 ([nm = t] ++ others))))]
                  (fn [u ::_] _ => return None)
                  fl_others
        fun innerCases
                [others ::_] [nm ::_] [t]
                [[nm] ~ others]
                fl_others (pf : Eq.t _ _)
                (f : tf1 t -> tf2 t -> f (tf3 t)) (x : tf1 t) =
            Eq.cast pf
                    [fn r =>
                        variant (map tf2 r)
                        -> f (option (variant (map tf3 r)))]
                    (cases (@nones ! fl_others
                             ++ {nm =
                                  fn (y : tf2 t) =>
                                     Monad.mp (Some <<< make [nm]) (f x y)}))
    in
        @@cases [map tf1 r] [_]
                (@mapNm [fn t => tf1 t -> tf2 t -> f (tf3 t)]
                        [fn t =>
                            tf1 t ->
                            variant (map tf2 r)
                            -> f (option (variant (map tf3 r)))]
                        fl
                        @@innerCases
                        fs)
    end

fun diag
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
        [r ::: {K}] (fl : folder r)
    : $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r) ->
      variant (map tf1 r) ->
      variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @@diagTraverse [tf1] [tf2] [tf3] [r] fl [fn t => t] monad_ident

fun diagU
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (f : t ::: K -> tf1 t -> tf2 t -> tf3 t)
    : variant (map tf1 r) ->
      variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @diag [tf1] [tf2] [tf3] fl
          (@map0 [fn t => tf1 t -> tf2 t -> tf3 t] (fn [t ::_] => f) fl)
