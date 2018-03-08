signature Types = sig
    type tunit = transaction unit
    con compose = K1 ==> K2 ==> K3 ==>
     fn (f :: K2 -> K3) (g :: K1 -> K2) (x :: K1) => f (g x)
    con forget = K ==> fn (t :: K) => ()
    con const = K1 ==> K2 ==> fn (t1 :: K1) => fn (t2 :: K2) => t1
    type void = variant []
end

structure T : Types = struct end

open T

fun ident [t] (x : t) = x

fun const [a] [b] (x : a) _ = x

fun on [a] [b] [c] (op : b -> b -> c) (f : a -> b) x y = op (f x) (f y)

fun zip [a] [b] [c] (f : a -> b -> c) (xs : list a) (ys : list b) : list c =
    case (xs, ys) of
        ([], _) => []
      | (_, []) => []
      | (x :: xs', y :: ys') => f x y :: zip f xs' ys'

fun impossible [t] loc : t =
    error <xml>The allegedly impossible has occurred at {[loc]}.</xml>

val monad_ident =
    mkMonad {Return = @@ident,
             Bind = fn [t1] [t2] (x : t1) (f : t1 -> t2) => f x}

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

fun when [m] (_ : monad m) (b : bool) (action : m unit) =
    if b then action else return ()

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

val contradiction = fn [t] => cases {}
