datatype sl a =
    SlNil
  | SlCons of {SlCarq : signal (option a), SlCdr : signal (sl a)}

con t a = signal (sl a)

fun mp [a] [b] (f : a -> b) =
    let
        fun goSl sl =
            case sl of
                SlNil => SlNil
              | SlCons cons =>
                SlCons {SlCarq = Monad.mp (Option.mp f) cons.SlCarq,
                        SlCdr = goT cons.SlCdr}
        and goT t = Monad.mp goSl t
    in
        goT
    end

fun foldl [a] [b] (f : a -> b -> b) (z : b) =
    let
        fun goSl sl =
            case sl of
                SlNil => return z
              | SlCons cons =>
                carq <- cons.SlCarq;
                cdr <- goT cons.SlCdr;
                return (case carq of
                            None => cdr
                          | Some car => (f car cdr))
        and goT t = bind t goSl
    in
        goT
    end

fun mapX [a] [ctx] [[Dyn] ~ ctx] (f : a -> xml ([Dyn] ++ ctx) [] []) =
    let
        fun goSl sl =
            case sl of
                SlNil => <xml></xml>
              | SlCons cons => <xml>
                <dyn signal={Monad.mp (compose (Option.get <xml></xml>)
                                               (Option.mp f))
                                      cons.SlCarq}/>
                {goT cons.SlCdr}
              </xml>
        and goT t = <xml><dyn signal={Monad.mp goSl t}/></xml>
    in
        goT
    end
