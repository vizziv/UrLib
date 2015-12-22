signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
end

structure T : Types = struct end
open T

fun id [t] (x : t) = x

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

fun casesU [K] [t ::: Type] [r ::: {K}] (fl : folder r) =
    @@cases [map (fn _ => t) r] [t]
            (@map0 [fn _ => t -> t] (fn [ignore ::_] => id) fl)

fun proj [nm ::_] [t] [drop] [[nm] ~ drop] (xs : $([nm = t] ++ drop)) = xs.nm

fun projs [keep] [drop] [keep ~ drop] (xs : $(keep ++ drop)) = xs --- drop

fun curry [have] [need] [t] [have ~ need]
          (f : $(have ++ need) -> t) (xs : $have) (ys : $need) =
    f (xs ++ ys)

fun snoc [ts] (xs : $ts) [nm :: Name] [t] [[nm] ~ ts] (x : t) = xs ++ {nm = x}

fun spawnListener [t] (action : t -> transaction unit) (chan : channel t) =
    let
        fun listen () = x <- recv chan; action x; listen ()
    in
        spawn (listen ())
    end

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
           (f : others :: {K} -> nm :: Name -> t ::: K
                -> [[nm] ~ others] => folder others
                -> tf ([nm = t] ++ others) t)
           [r ::: {K}] (fl : folder r)
    : $(map (tf r) r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> {FlDone : folder done,
                   R : $(map (tf (done ++ todo)) done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> {FlDone : folder done,
                          R : $(map (tf (done ++ todo)) done)})
               [todo :: {K}]
               [done ++ [nm = t] ~ todo] (flTodo : folder todo) =>
               let
                   val acc = @acc [[nm = t] ++ todo]
                                  ! (@Folder.cons [nm] [t] ! flTodo)
               in
                   {FlDone = @Folder.cons [nm] [t] ! acc.FlDone,
                    R = acc.R
                        ++ {nm = @f [done ++ todo] [nm]
                                    ! (@Folder.concat ! acc.FlDone flTodo)}}
               end)
           (fn [todo :: {K}] [[] ~ todo] (flTodo : folder todo) =>
               {FlDone = Folder.nil, R = {}})
           fl [[]] ! Folder.nil).R

fun mapNm [K] [tf1 :: K -> Type] [tf2 :: {K} -> K -> Type]
          (f : others :: {K} -> nm :: Name -> t ::: K
               -> [[nm] ~ others] => folder others
               -> tf1 t -> tf2 ([nm = t] ++ others) t)
          [r ::: {K}] (fl : folder r)
    : $(map tf1 r) -> $(map (tf2 r) r) =
    (@fold [fn done :: {K} =>
               todo :: {K} -> [done ~ todo] => folder todo
               -> {FlDone : folder done,
                   MapF : $(map tf1 done) -> $(map (tf2 (done ++ todo)) done)}]
           (fn [nm :: Name] [t :: K] [done :: {K}] [[nm] ~ done]
               (acc : todo :: {K} -> [done ~ todo] => folder todo
                      -> {FlDone : folder done,
                          MapF : $(map tf1 done)
                                 -> $(map (tf2 (done ++ todo)) done)})
               [todo :: {K}]
               [done ++ [nm = t] ~ todo] (fl_todo : folder todo) =>
               let
                   val acc = @acc [[nm = t] ++ todo]
                                  ! (@Folder.cons [nm] [t] ! fl_todo)
               in
                   {FlDone = @Folder.cons [nm] [t] ! acc.FlDone,
                    MapF = fn (x : $(map tf1 (done ++ [nm = t]))) =>
                              acc.MapF (x -- nm)
                              ++ {nm = @f [done ++ todo] [nm]
                                        ! (@Folder.concat ! acc.FlDone fl_todo)
                                        x.nm}}
               end)
           (fn [todo :: {K}] [[] ~ todo] (fl_todo : folder todo) =>
               {FlDone = Folder.nil, MapF = fn {} => {}})
           fl [[]] ! Folder.nil).MapF

fun casesMap [K] [tf1 :: K -> Type] [tf2 :: K -> Type]
             (f : t ::: K -> tf1 t -> tf2 t)
             [r ::: {K}] (fl : folder r)
    : variant (map tf1 r) -> variant (map tf2 r) =
    @@cases [map tf1 r] [_]
            (@mapNm0 [fn r t => tf1 t -> variant (map tf2 r)]
                     (fn [others ::_] [nm ::_] [t]
                         [[nm] ~ others] _ =>
                         compose (make [nm]) f)
                     fl)

fun casesDiag [K] [tf1 :: K -> Type] [tf2 :: K -> Type] [tf3 :: K -> Type]
              (f : t ::: K -> tf1 t -> tf2 t -> tf3 t)
              [r ::: {K}] (fl : folder r)
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
                (@mapNm0 [fn r t =>
                             tf1 t -> variant (map tf2 r)
                             -> option (variant (map tf3 r))]
                         (fn [others ::_] [nm ::_] [t]
                             [[nm] ~ others] fl_others
                             (x : tf1 t) =>
                             cases (@nones ! fl_others
                                    ++ {nm = fn (y : tf2 t) =>
                                                Some (make [nm] (f x y))}))
                         fl)
    end
