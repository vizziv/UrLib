signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    type void
end

structure T : Types
    where type void = variant [] = struct end

open T

fun id [t] (x : t) = x

fun on [a] [b] [c] (op : b -> b -> c) (f : a -> b) x y = op (f x) (f y)

fun zip [a] [b] [c] (f : a -> b -> c) (xs : list a) (ys : list b) : list c =
    case (xs, ys) of
        ([], _) => []
      | (_, []) => []
      | (x :: xs', y :: ys') => f x y :: zip f xs' ys'

fun impossible [t] loc : t =
    error <xml>The allegedly impossible has occurred at {[loc]}.</xml>

val identity_monad =
    mkMonad {Return = @@id, Bind = fn [t1] [t2] (x : t1) (f : t1 -> t2) => f x}

fun bit b = if b then 1 else 0

fun maximum [t] (_ : ord t) : t -> list t -> t = List.foldl max
fun minimum [t] (_ : ord t) : t -> list t -> t = List.foldl min

fun proj [nm ::_] [t] [drop] [[nm] ~ drop] (xs : $([nm = t] ++ drop)) = xs.nm

val proj1 = fn [nm] [t] => proj [nm]

fun projs [keep] [drop] [keep ~ drop] (xs : $(keep ++ drop)) = xs --- drop

fun rename [nm1 ::_] [nm2 ::_] [t] [ts] [[nm1] ~ ts] [[nm2] ~ ts]
           (xs : $([nm1 = t] ++ ts)) =
    xs -- nm1 ++ {nm2 = xs.nm1}

fun curry [have] [need] [t] [have ~ need]
          (f : $(have ++ need) -> t) (xs : $have) (ys : $need) =
    f (xs ++ ys)

fun snoc [ts] (xs : $ts) [nm :: Name] [t] [[nm] ~ ts] (x : t) = xs ++ {nm = x}

fun injqs [keep] [drop] [keep ~ drop]
          (fl_keep : folder keep) (fl_drop : folder drop)
          (xs : $keep) =
    @mp [ident] [option] @@Some fl_keep xs
    ++ @map0 [option] (fn [t ::_] => None) fl_drop

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

con exDisj = K ==>
 fn tf :: ({K} -> {K} -> Type) =>
    res ::: Type
    -> (choice1 :: {K} -> choice2 :: {K} -> [choice1 ~ choice2]
        => tf choice1 choice2 -> res)
    -> res

fun exDisj_intro [K] [tf :: {K} -> {K} -> Type]
                 [choice1 :: {K}] [choice2 :: {K}] [choice1 ~ choice2]
                 (body : tf choice1 choice2) : exDisj tf =
 fn [res]
    (f : choice1 :: {K} -> choice2 :: {K} -> [choice1 ~ choice2]
         => tf choice1 choice2 -> res) =>
    f [choice1] [choice2] body

fun exDisj_elim [K] [tf ::: {K} -> {K} -> Type]
                (v : exDisj tf) [res ::: Type] =
    @@v [res]

fun mapNm0 [K] [tf :: {K} -> K -> Type]
           [r ::: {K}] (fl : folder r)
           (f : others :: {K} -> nm :: Name -> t ::: K
                -> [[nm] ~ others] => folder others
                -> Eq.t ([nm = t] ++ others) r
                -> tf ([nm = t] ++ others) t)
    : $(map (tf r) r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> Eq.t (done ++ todo) r
               -> {FlDone : folder done,
                   MapF : $(map (tf (done ++ todo)) done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> Eq.t (done ++ todo) r
                      -> {FlDone : folder done,
                          MapF : $(map (tf (done ++ todo)) done)})
               [todo :: {K}] [done ++ [nm = t] ~ todo] (fl_todo : folder todo)
               (pf : Eq.t (done ++ [nm = t] ++ todo) r) =>
               let
                   val acc = @acc [[nm = t] ++ todo] !
                                  (@Folder.cons [nm] [t] ! fl_todo)
                                  pf
               in
                   {FlDone = @Folder.cons [nm] [t] ! acc.FlDone,
                    MapF = acc.MapF
                        ++ {nm = @f [done ++ todo] [nm] !
                                    (@Folder.concat ! acc.FlDone fl_todo)
                                    pf}}
               end)
           (fn [todo :: {K}] [[] ~ todo] (_ : folder todo) _ =>
               {FlDone = Folder.nil, MapF = {}})
           fl [[]] ! Folder.nil Eq.refl).MapF

fun mapNm [K] [tf1 :: K -> Type] [tf2 :: {K} -> K -> Type]
          [r ::: {K}] (fl : folder r)
          (f : others :: {K} -> nm :: Name -> t ::: K
               -> [[nm] ~ others] => folder others
               -> Eq.t ([nm = t] ++ others) r
               -> tf1 t -> tf2 ([nm = t] ++ others) t)
    : $(map tf1 r) -> $(map (tf2 r) r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> Eq.t (done ++ todo) r
               -> {FlDone : folder done,
                   MapF : $(map tf1 done) -> $(map (tf2 (done ++ todo)) done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> Eq.t (done ++ todo) r
                      -> {FlDone : folder done,
                          MapF : $(map tf1 done)
                                 -> $(map (tf2 (done ++ todo)) done)})
               [todo :: {K}] [done ++ [nm = t] ~ todo] (fl_todo : folder todo)
               (pf : Eq.t (done ++ [nm = t] ++ todo) r) =>
               let
                   val acc =
                       @acc [[nm = t] ++ todo] !
                            (@Folder.cons [nm] [t] ! fl_todo)
                            pf
               in
                   {FlDone = @Folder.cons [nm] [t] ! acc.FlDone,
                    MapF = fn (x : $(map tf1 (done ++ [nm = t]))) =>
                              acc.MapF (x -- nm)
                              ++ {nm = @f [done ++ todo] [nm] !
                                          (@Folder.concat ! acc.FlDone fl_todo)
                                          pf x.nm}}
               end)
           (fn [todo :: {K}] [[] ~ todo] _ _ => {FlDone = Folder.nil, MapF = fn {} => {}})
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

fun field [nm ::_] [ts] [[nm] ~ ts] [a] [b]
          (f : a -> b) (r : $([nm = a] ++ ts)) =
    r -- nm ++ {nm = f r.nm}

fun choice [nm ::_] [ts] [[nm] ~ ts] (fl : folder ts) [a] [b] (f : a -> b)
    : variant ([nm = a] ++ ts) -> variant ([nm = b] ++ ts) =
    @@cases [[nm = a] ++ ts] [_]
            ((@mapNm0 [fn r t => t -> variant r]
                      (@Folder.cons [nm] [b] ! fl)
                      (fn [others ::_] [nm' ::_] [t] [[nm'] ~ others] _ _ =>
                          make [nm']))
             -- nm ++ {nm = fn x => make [nm] (f x)})

fun compose [f] [g] (mpf : t f) (mpg : t g) [a] [b] = Top.compose mpf mpg

end

fun casesGet [K] [r ::: {K}] (fl : folder r) [t ::: Type] =
    @@cases [map (fn _ => t) r] [t]
            (@map0 [fn _ => t -> t] (fn [ignore ::_] => id) fl)

val contradiction = fn [t] => cases {}

fun casesFunctor [r ::: {Type}] (fl : folder r)
                 [f ::: Type -> Type] (_ : Functor.t f) =
    @@cases [map f r] [f (variant r)]
            (@mapNm0 [fn r t => f t -> f (variant r)] fl
                     (fn [others ::_] [nm ::_] [t] [[nm] ~ others ] _ _ =>
                         Functor.mp (make [nm])))

fun casesMap [K] [tf1 :: K -> Type] [tf2 :: K -> Type]
             [r ::: {K}] (fl : folder r)
             (fs : $(map (fn t :: K => tf1 t -> tf2 t) r))
    : variant (map tf1 r) -> variant (map tf2 r) =
    @@cases [map tf1 r] [_]
            (@mapNm [fn t => tf1 t -> tf2 t]
                    [fn r t => tf1 t -> variant (map tf2 r)]
                    fl
                    (fn [others ::_] [nm ::_] [t]
                        [[nm] ~ others] _ _ =>
                        compose (make [nm]))
                    fs)

fun casesMapU [K] [tf1 :: K -> Type] [tf2 :: K -> Type]
              [r ::: {K}] (fl : folder r)
              (f : t ::: K -> tf1 t -> tf2 t)
    : variant (map tf1 r) -> variant (map tf2 r) =
    @casesMap [tf1] [tf2] fl
              (@map0 [fn t => tf1 t -> tf2 t] (fn [t ::_] => f) fl)

fun casesTraverse [K] [tf1 :: K -> Type] [tf2 :: K -> Type]
                  [r ::: {K}] (fl : folder r)
                  [f ::: Type -> Type] (_ : Functor.t f)
                  (fs : $(map (fn t :: K => tf1 t -> f (tf2 t)) r))
    : variant (map tf1 r) -> f (variant (map tf2 r)) =
    @@cases [map tf1 r] [_]
            (@mapNm [fn t => tf1 t -> f (tf2 t)]
                    [fn r t => tf1 t -> f (variant (map tf2 r))]
                    fl
                    (fn [others ::_] [nm ::_] [t]
                        [[nm] ~ others] _ _ =>
                        compose (Functor.mp (make [nm])))
                    fs)

fun casesDiagTraverse [K] [tf1 :: K -> Type] [tf2 :: K -> Type]
                      [tf3 :: K -> Type]
                      [r ::: {K}] (fl : folder r)
                      [f ::: Type -> Type] (_ : monad f)
                      (fs : $(map (fn t :: K => tf1 t -> tf2 t -> f (tf3 t))
                                  r))
    : variant (map tf1 r) -> variant (map tf2 r)
      -> f (option (variant (map tf3 r))) =
    let
        fun nones [others] [nm] [t]
                  [[nm] ~ others] (fl_others : folder others) =
            @map0 [fn u =>
                      tf2 u ->
                      f (option (variant (map tf3 ([nm = t] ++ others))))]
                  (fn [u ::_] _ => return None)
                  fl_others
    in
        @@cases [map tf1 r] [_]
                (@mapNm [fn t => tf1 t -> tf2 t -> f (tf3 t)]
                        [fn r t =>
                            tf1 t -> variant (map tf2 r)
                            -> f (option (variant (map tf3 r)))]
                        fl
                        (fn [others ::_] [nm ::_] [t]
                            [[nm] ~ others] fl_others _
                            (f : tf1 t -> tf2 t -> f (tf3 t)) (x : tf1 t) =>
                            cases (@nones ! fl_others
                                   ++ {nm = fn (y : tf2 t) =>
                                               z <- f x y;
                                               return (Some (make [nm] z))}))
                        fs)
    end

fun casesDiag [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
              [r ::: {K}] (fl : folder r)
    : $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r)
      -> variant (map tf1 r) -> variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @@casesDiagTraverse [tf1] [tf2] [tf3] [r] fl [fn t => t] identity_monad

fun casesDiagU [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
               [r ::: {K}] (fl : folder r)
               (f : t ::: K -> tf1 t -> tf2 t -> tf3 t)
    : variant (map tf1 r) -> variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @casesDiag [tf1] [tf2] [tf3] fl
               (@map0 [fn t => tf1 t -> tf2 t -> tf3 t] (fn [t ::_] => f) fl)
