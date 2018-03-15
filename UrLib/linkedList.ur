open Prelude

datatype llSignals a =
    SglNil
  | SglCons of {Carq : signal (option a), Cdr : signal (llSignals a)}

con signals a = signal (llSignals a)

fun mp [a] [b] (f : a -> b) =
    let
        fun goLl ll =
            case ll of
                SglNil => SglNil
              | SglCons cons =>
                SglCons {Carq = Monad.mp (Option.mp f) cons.Carq,
                         Cdr = goSgl cons.Cdr}
        and goSgl sgl = Monad.mp goLl sgl
    in
        goSgl
    end

fun foldl [a] [b] (f : a -> b -> b) (z : b) =
    let
        fun goLl ll =
            case ll of
                SglNil => return z
              | SglCons cons =>
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
                SglNil => xempty
              | SglCons cons => <xml>
                {xdyn (Monad.mp (Option.get xempty <<< Option.mp f)
                                cons.Carq)}
                {goSgl cons.Cdr}
              </xml>
              and goSgl sgl = xdyn (Monad.mp goLl sgl)
    in
        (* The extra [xdyn] layer forces evaluation of the linked list on the
           client side, which is necessary for code generation. *)
        xdyn <<< return <<< goSgl
    end

datatype llSources a =
    SrcNil
  | SrcCons of {Carq : source (option a), Cdr : source (llSources a)}

con sources a =
    {First : source (llSources a),
     Last : source (source (llSources a))}

fun mk [a] (fold : b ::: Type ->
                   (a -> b -> transaction b) -> b
                   -> transaction b)
    : transaction (sources a) =
    first <- source SrcNil;
    let
        fun go (x : a) last =
            carq <- source (Some x);
            nil <- source SrcNil;
            set last (SrcCons {Carq = carq, Cdr = nil});
            return nil
    in
        last <- bind (fold go first) source;
        return {First = first, Last = last}
    end

fun value [a] (srcs : sources a) : signals a =
    let
        fun goLl ll =
            case ll of
                SrcNil => SglNil
              | SrcCons cons =>
                SglCons {Carq = signal cons.Carq, Cdr = goSrc cons.Cdr}
        and goSrc src = Monad.mp goLl (signal src)
    in
        goSrc srcs.First
    end

fun insert [a] (x : a) (srcs : sources a) =
    (* Last should always point to SrcNil, but in case it somehow points to a
       SrcCons, this inserts in the middle rather than dropping the end. *)
    lastOld <- get srcs.Last;
    lastNew <- bind (get lastOld) source;
    carq <- source (Some x);
    set lastOld (SrcCons {Carq = carq, Cdr = lastNew});
    set srcs.Last lastNew

fun iterPred [a] (f : source (option a) -> tunit) (p : a -> bool)
    : sources a -> tunit =
    let
        fun goLl ll =
            case ll of
                SrcNil => return ()
              | SrcCons cons =>
                carq <- get cons.Carq;
                (case carq of
                     None => return ()
                   | Some car => when (p car) (f cons.Carq));
                goSrc cons.Cdr
        and goSrc src = bind (get src) goLl
    in
        goSrc <<< Record.proj [#First]
    end

fun update [a] (f : a -> a) =
    iterPred (fn src => bind (get src) (Option.mp f >>> set src))

val delete = fn [a] => iterPred (fn src => set src None)

structure Debug = struct
    fun print [a] (_ : show a) (srcs : sources a) =
        let
            fun goLl ll =
                case ll of
                    SrcNil => return ""
                  | SrcCons cons =>
                    carq <- get cons.Carq;
                    acc <- goSrc cons.Cdr;
                    return ((case carq of
                                 None => "  ~"
                               | Some car => "  [" ^ show car ^ "]")
                            ^ acc)
            and goSrc src = bind (get src) goLl
        in
            bind (goSrc srcs.First) debug
        end
end
