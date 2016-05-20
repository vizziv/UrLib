open Prelude

structure Signal = struct

    datatype ll a =
             Nil
           | Cons of {Carq : signal (option a), Cdr : signal (ll a)}

    con t a = signal (ll a)

    fun mp [a] [b] (f : a -> b) =
        let
            fun goLl ll =
                case ll of
                    Nil => Nil
                  | Cons cons =>
                    Cons {Carq = Monad.mp (Option.mp f) cons.Carq,
                          Cdr = goSgl cons.Cdr}
            and goSgl sgl = Monad.mp goLl sgl
        in
            goSgl
        end

    fun foldl [a] [b] (f : a -> b -> b) (z : b) =
        let
            fun goLl ll =
                case ll of
                    Nil => return z
                  | Cons cons =>
                    carq <- cons.Carq;
                    cdr <- goSgl cons.Cdr;
                    return (case carq of
                                None => cdr
                              | Some car => (f car cdr))
            and goSgl sgl = bind sgl goLl
        in
            goSgl
        end

    fun mapX [a] [ctx] [[Dyn] ~ ctx] (f : a -> xml ([Dyn] ++ ctx) [] []) =
        let
            fun goLl ll =
                case ll of
                    Nil => <xml></xml>
                  | Cons cons => <xml>
                    {xdyn (Monad.mp (compose (Option.get <xml></xml>)
                                             (Option.mp f))
                                    cons.Carq)}
                    {goSgl cons.Cdr}
                  </xml>
            and goSgl sgl = xdyn (Monad.mp goLl sgl)
        in
            goSgl
        end

end

structure Source = struct

    datatype ll a =
             Nil
           | Cons of {Carq : source (option a), Cdr : source (ll a)}

    con t a = {First : source (ll a), Last : source (source (ll a))}

    fun value [a] (t : t a) : Signal.t a =
        let
            fun goLl ll =
                case ll of
                    Nil => Signal.Nil
                  | Cons cons =>
                    Signal.Cons {Carq = signal cons.Carq, Cdr = goSrc cons.Cdr}
            and goSrc src = Monad.mp goLl (signal src)
        in
            goSrc t.First
        end

    fun insert [a] (x : a) (t : t a) =
        (* Last should always point to Nil, but in case it somehow points to
           Cons, this inserts in the middle rather than dropping the end. *)
        cdr <- bind (bind (get t.Last) get) source;
        carq <- source (Some x);
        lastNew <- source (Cons {Carq = carq, Cdr = cdr});
        set t.Last lastNew

    fun iterPred [a] (f : source (option a) -> tunit) (p : a -> bool)
        : t a -> tunit =
        let
            fun goLl ll =
                case ll of
                    Nil => return ()
                  | Cons cons =>
                    carq <- get cons.Carq;
                    (case carq of
                         None => return ()
                       | Some car =>
                         if p car then
                             f cons.Carq
                         else
                             return ());
                    goSrc cons.Cdr
            and goSrc src = bind (get src) goLl
        in
            compose goSrc (proj [#First])
        end

    fun update [a] (f : a -> a) =
        iterPred (fn src => bind (get src) (compose (set src) (Option.mp f)))

    val delete = fn [a] => iterPred (fn src => set src None)

end
