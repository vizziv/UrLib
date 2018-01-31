signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    type void = variant []
end

structure T : Types = struct end

open T

fun id [t] (x : t) = x

fun const [a] [b] (x : a) _ = x

fun on [a] [b] [c] (op : b -> b -> c) (f : a -> b) x y = op (f x) (f y)

fun zip [a] [b] [c] (f : a -> b -> c) (xs : list a) (ys : list b) : list c =
    case (xs, ys) of
        ([], _) => []
      | (_, []) => []
      | (x :: xs', y :: ys') => f x y :: zip f xs' ys'

fun impossible [t] loc : t =
    error <xml>The allegedly impossible has occurred at {txt loc}.</xml>

val monad_ident =
    mkMonad {Return = @@id, Bind = fn [t1] [t2] (x : t1) (f : t1 -> t2) => f x}

fun bit b = if b then 1 else 0

fun maximum [t] (_ : ord t) : t -> list t -> t = List.foldl max
fun minimum [t] (_ : ord t) : t -> list t -> t = List.foldl min

fun mapiPartial [a] [b] (f : int -> a -> option b) =
    let
        fun mp' n acc ls =
            case ls of
                [] => List.rev acc
              | x :: ls => mp' (n+1) (case f n x of
                                          None => acc
                                        | Some y => y :: acc) ls
    in
        mp' 0 []
    end

fun distinct [t] (_ : eq t) (_ : ord t) (xs : list t) =
    let
        fun check xs =
            case xs of
                x0 :: x1 :: xs => if x0 = x1 then False else check (x1 :: xs)
              | _ => True
    in
        check (List.sort le xs)
    end

fun spawnListener [t] (action : t -> tunit) (chan : channel t) =
    let
        fun loop () = x <- recv chan; action x; loop ()
    in
        spawn (loop ())
    end

val xempty = fn [ctx] => <xml></xml> : xml ctx [] []

fun xdyn [ctx] [[Dyn] ~ ctx] sgl = <xml><dyn signal={sgl}/></xml>

fun xactive code = <xml><active code={code}/></xml>

fun xaction code = xactive (code; return xempty)

fun mapNm0
        [K]
        [tf :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (f : others :: {K} -> nm :: Name -> t ::: K -> [[nm] ~ others] =>
         folder others -> Eq.t ([nm = t] ++ others) r
         -> tf t)
    : $(map tf r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] =>
               folder todo -> Eq.t (done ++ todo) r
               -> {Fl_done : folder done,
                   MapF : $(map tf done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] =>
                      folder todo -> Eq.t (done ++ todo) r
                      -> {Fl_done : folder done,
                          MapF : $(map tf done)})
               [todo :: {K}] [done ++ [nm = t] ~ todo] (fl_todo : folder todo)
               (pf : Eq.t (done ++ [nm = t] ++ todo) r) =>
               let
                   val acc = @acc [[nm = t] ++ todo] !
                                  (@Folder.cons [nm] [t] ! fl_todo)
                                  pf
               in
                   {Fl_done = @Folder.cons [nm] [t] ! acc.Fl_done,
                    MapF =
                        acc.MapF
                        ++ {nm = @f [done ++ todo] [nm] !
                                    (@Folder.concat ! acc.Fl_done fl_todo) pf}}
               end)
           (fn [todo :: {K}] [[] ~ todo] (_ : folder todo) _ =>
               {Fl_done = Folder.nil, MapF = {}})
           fl [[]] ! Folder.nil Eq.refl).MapF

fun mapNm
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (f : others :: {K} -> nm :: Name -> t ::: K  ->[[nm] ~ others] =>
         folder others -> Eq.t ([nm = t] ++ others) r ->
         tf1 t
         -> tf2 t)
    : $(map tf1 r)
      -> $(map tf2 r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> Eq.t (done ++ todo) r
               -> {Fl_done : folder done,
                   MapF : $(map tf1 done) -> $(map tf2 done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> Eq.t (done ++ todo) r
                      -> {Fl_done : folder done,
                          MapF : $(map tf1 done)
                                 -> $(map tf2 done)})
               [todo :: {K}] [done ++ [nm = t] ~ todo] (fl_todo : folder todo)
               (pf : Eq.t (done ++ [nm = t] ++ todo) r) =>
               let
                   val acc =
                       @acc [[nm = t] ++ todo] !
                            (@Folder.cons [nm] [t] ! fl_todo)
                            pf
               in
                   {Fl_done = @Folder.cons [nm] [t] ! acc.Fl_done,
                    MapF =
                     fn (x : $(map tf1 (done ++ [nm = t]))) =>
                        acc.MapF (x -- nm)
                        ++ {nm = @f [done ++ todo] [nm] !
                                    (@Folder.concat ! acc.Fl_done fl_todo) pf
                                    x.nm}}
               end)
           (fn [todo :: {K}] [[] ~ todo] _ _ =>
               {Fl_done = Folder.nil, MapF = fn {} => {}})
           fl [[]] ! Folder.nil Eq.refl).MapF

fun cases [ts ::: {Type}] [u] (fs : $(map (fn t => t -> u) ts)) v = match v fs

structure Functor  : sig
    class t :: (Type -> Type) -> Type
    val mk : f ::: (Type -> Type)
             -> (a ::: Type -> b ::: Type -> (a -> b) -> f a -> f b)
             -> t f
    val mp : f ::: (Type -> Type) -> t f ->
             a ::: Type -> b ::: Type -> (a -> b) -> f a -> f b
    val monad : f ::: (Type -> Type) -> monad f -> t f
    val list : t list
    val field : nm :: Name -> ts ::: {Type} -> [[nm] ~ ts]
                => t (fn t => $([nm = t] ++ ts))
    val choice : nm :: Name -> ts ::: {Type} -> [[nm] ~ ts] => folder ts
                 -> t (fn t => variant ([nm = t] ++ ts))
    val compose : f ::: (Type -> Type) -> g ::: (Type -> Type)
                  -> t f -> t g -> t (compose f g)
end = struct

con t (f :: Type -> Type) = a ::: Type -> b ::: Type -> (a -> b) -> f a -> f b

val mk = fn [f] => id

val mp = fn [f] => id

val monad = @@Monad.mp

val list = @@List.mp

fun field
        [nm ::_] [ts] [[nm] ~ ts]
        [a] [b]
        (f : a -> b) (r : $([nm = a] ++ ts)) =
    r -- nm ++ {nm = f r.nm}

fun choice
        [nm ::_] [ts] [[nm] ~ ts]
        (fl : folder ts)
        [a] [b]
        (f : a -> b)
    : variant ([nm = a] ++ ts) -> variant ([nm = b] ++ ts) =
    @@cases [[nm = a] ++ ts] [_]
            ((@mapNm0 [fn t => t -> variant ([nm = b] ++ ts)]
                      (@Folder.cons [nm] [b] ! fl)
                      (fn [others ::_] [nm' ::_] [t] [[nm'] ~ others] _ pf =>
                          Eq.make [nm'] pf))
             -- nm ++ {nm = fn x => make [nm] (f x)})

fun compose [f] [g] (mpf : t f) (mpg : t g) [a] [b] = Top.compose mpf mpg

end

fun casesGet [K] [r ::: {K}] (fl : folder r) [t ::: Type] =
    @@cases [map (fn _ => t) r] [t]
            (@map0 [fn _ => t -> t] (fn [ignore ::_] => id) fl)

val contradiction = fn [t] => cases {}

fun casesFunctor
        [r ::: {Type}] (fl : folder r)
        [f ::: Type -> Type] (_ : Functor.t f) =
    @@cases [map f r] [f (variant r)]
            (@mapNm0 [fn t => f t -> f (variant r)] fl
                     (fn [others ::_] [nm ::_] [t] [[nm] ~ others ] _ pf =>
                         Functor.mp (Eq.make [nm] pf)))

fun casesMap
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

fun casesMapU
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (f : t ::: K -> tf1 t -> tf2 t)
    : variant (map tf1 r)
      -> variant (map tf2 r) =
    @casesMap [tf1] [tf2] fl
              (@map0 [fn t => tf1 t -> tf2 t] (fn [t ::_] => f) fl)

fun casesTraverse
    [K]
    [tf1 :: K -> Type] [tf2 :: K -> Type]
    [r ::: {K}] (fl : folder r)
    [f ::: Type -> Type] (_ : Functor.t f)
    (fs : $(map (fn t :: K => tf1 t -> f (tf2 t)) r))
    : variant (map tf1 r) -> f (variant (map tf2 r)) =
    @@cases [map tf1 r] [_]
            (@mapNm [fn t => tf1 t -> f (tf2 t)]
                    [fn t => tf1 t -> f (variant (map tf2 r))]
                    fl
                    (fn [others ::_] [nm ::_] [t]
                        [[nm] ~ others] _ pf =>
                        compose (Functor.mp (Eq.makeMap [nm] [tf2] pf)))
                    fs)

fun casesDiagTraverse
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

fun casesDiag
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
        [r ::: {K}] (fl : folder r)
    : $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r) ->
      variant (map tf1 r) ->
      variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @@casesDiagTraverse [tf1] [tf2] [tf3] [r] fl [fn t => t] monad_ident

fun casesDiagU
        [K]
        [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
        [r ::: {K}] (fl : folder r)
        (f : t ::: K -> tf1 t -> tf2 t -> tf3 t)
    : variant (map tf1 r) ->
      variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @casesDiag [tf1] [tf2] [tf3] fl
               (@map0 [fn t => tf1 t -> tf2 t -> tf3 t] (fn [t ::_] => f) fl)
