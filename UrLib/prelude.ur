signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    con equal :: K --> K -> K -> Type
end

structure T : Types where
              con equal = K ==>
               fn a b =>
                  {CastL : f :: (K -> Type) -> f b -> f a,
                   CastR : f :: (K -> Type) -> f a -> f b} = struct end
open T

fun id [t] (x : t) = x

val refl : K --> a ::: K -> equal a a =
 fn [K] [a] =>
    {CastL = fn [tf :: K -> Type] => id,
     CastR = fn [tf :: K -> Type] => id}

fun castL [K] [a ::: K] [b ::: K] (pf : equal a b) = pf.CastL
fun castR [K] [a ::: K] [b ::: K] (pf : equal a b) = pf.CastR

fun maximum [t] (_ : ord t) : t -> list t -> t = List.foldl max
fun minimum [t] (_ : ord t) : t -> list t -> t = List.foldl min

fun distinct [t] (_ : eq t) (_ : ord t) (xs : list t) =
    let
        fun check xs =
            case xs of
                x0 :: x1 :: xs => if x0 = x1 then False else check (x1 :: xs)
              | _ => True
    in
        check (List.sort le xs)
    end

fun cases [ts ::: {Type}] [u] (fs : $(map (fn t => t -> u) ts)) v = match v fs

fun casesGet [K] [t ::: Type] [r ::: {K}] (fl : folder r) =
    @@cases [map (fn _ => t) r] [t]
            (@map0 [fn _ => t -> t] (fn [ignore ::_] => id) fl)

fun proj [nm ::_] [t] [drop] [[nm] ~ drop] (xs : $([nm = t] ++ drop)) = xs.nm

fun projs [keep] [drop] [keep ~ drop] (xs : $(keep ++ drop)) = xs --- drop

fun curry [have] [need] [t] [have ~ need]
          (f : $(have ++ need) -> t) (xs : $have) (ys : $need) =
    f (xs ++ ys)

fun snoc [ts] (xs : $ts) [nm :: Name] [t] [[nm] ~ ts] (x : t) = xs ++ {nm = x}

fun spawnListener [t] (action : t -> tunit) (chan : channel t) =
    let
        fun listen () = x <- recv chan; action x; listen ()
    in
        spawn (listen ())
    end

fun spawnSignal [t] (chan : channel t) : transaction (signal (option t)) =
    src <- source None;
    spawnListener (compose (set src) Some) chan;
    return (signal src)

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

fun mapNm0 [K] [tf :: {K} -> K -> Type]
           [r ::: {K}] (fl : folder r)
           (f : others :: {K} -> nm :: Name -> t ::: K
                -> [[nm] ~ others] => folder others
                -> equal r ([nm = t] ++ others)
                -> tf ([nm = t] ++ others) t)
    : $(map (tf r) r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> equal r (done ++ todo)
               -> {FlDone : folder done,
                   MapF : $(map (tf (done ++ todo)) done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> equal r (done ++ todo)
                      -> {FlDone : folder done,
                          MapF : $(map (tf (done ++ todo)) done)})
               [todo :: {K}] [done ++ [nm = t] ~ todo] (flTodo : folder todo)
               (cast : equal r (done ++ [nm = t] ++ todo)) =>
               let
                   val acc = @acc [[nm = t] ++ todo] !
                                  (@Folder.cons [nm] [t] ! flTodo)
                                  cast
               in
                   {FlDone = @Folder.cons [nm] [t] ! acc.FlDone,
                    MapF = acc.MapF
                        ++ {nm = @f [done ++ todo] [nm] !
                                    (@Folder.concat ! acc.FlDone flTodo)
                                    cast}}
               end)
           (fn [todo :: {K}] [[] ~ todo] (_ : folder todo) _ =>
               {FlDone = Folder.nil, MapF = {}})
           fl [[]] ! Folder.nil refl).MapF

fun mapNm [K] [tf1 :: K -> Type] [tf2 :: {K} -> K -> Type]
          [r ::: {K}] (fl : folder r)
          (f : others :: {K} -> nm :: Name -> t ::: K
               -> [[nm] ~ others] => folder others
               -> equal r ([nm = t] ++ others)
               -> tf1 t -> tf2 ([nm = t] ++ others) t)
    : $(map tf1 r) -> $(map (tf2 r) r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> equal r (done ++ todo)
               -> {FlDone : folder done,
                   MapF : $(map tf1 done) -> $(map (tf2 (done ++ todo)) done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> equal r (done ++ todo)
                      -> {FlDone : folder done,
                          MapF : $(map tf1 done)
                                 -> $(map (tf2 (done ++ todo)) done)})
               [todo :: {K}] [done ++ [nm = t] ~ todo] (fl_todo : folder todo)
               (cast : equal r (done ++ [nm = t] ++ todo)) =>
               let
                   val acc =
                       @acc [[nm = t] ++ todo] !
                            (@Folder.cons [nm] [t] ! fl_todo)
                            cast
               in
                   {FlDone = @Folder.cons [nm] [t] ! acc.FlDone,
                    MapF = fn (x : $(map tf1 (done ++ [nm = t]))) =>
                              acc.MapF (x -- nm)
                              ++ {nm = @f [done ++ todo] [nm] !
                                          (@Folder.concat ! acc.FlDone fl_todo)
                                          cast x.nm}}
               end)
           (fn [todo :: {K}] [[] ~ todo] _ _ => {FlDone = Folder.nil, MapF = fn {} => {}})
           fl [[]] ! Folder.nil refl).MapF

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

fun casesDiag [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
              [r ::: {K}] (fl : folder r)
              (fs : $(map (fn t :: K => tf1 t -> tf2 t -> tf3 t) r))
    : variant (map tf1 r) -> variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    let
        fun nones [others] [nm] [t]
                  [[nm] ~ others] (fl_others : folder others) =
            @map0 [fn u =>
                      tf2 u -> option (variant (map tf3 ([nm = t] ++ others)))]
                  (fn [u ::_] _ => None)
                  fl_others
    in
        @@cases [map tf1 r] [_]
                (@mapNm [fn t => tf1 t -> tf2 t -> tf3 t]
                        [fn r t =>
                            tf1 t -> variant (map tf2 r)
                            -> option (variant (map tf3 r))]
                        fl
                        (fn [others ::_] [nm ::_] [t]
                            [[nm] ~ others] fl_others _
                            (f : tf1 t -> tf2 t -> tf3 t) (x : tf1 t) =>
                            cases (@nones ! fl_others
                                   ++ {nm = fn (y : tf2 t) =>
                                               Some (make [nm] (f x y))}))
                        fs)
    end

fun casesDiagU [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
               [r ::: {K}] (fl : folder r)
               (f : t ::: K -> tf1 t -> tf2 t -> tf3 t)
    : variant (map tf1 r) -> variant (map tf2 r)
      -> option (variant (map tf3 r)) =
    @casesDiag [tf1] [tf2] [tf3] fl
               (@map0 [fn t => tf1 t -> tf2 t -> tf3 t] (fn [t ::_] => f) fl)
