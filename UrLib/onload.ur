open Prelude

con t a = transaction {Server : a, Client : tunit}

val monad =
    let
        (* All returns and binds here use [transaction]. *)
        fun r [a] (x : a) : t a =
            return {Server = x, Client = return ()}
        fun b [a] [b] (tx : t a) (f : a -> t b) : t b =
            x <- tx;
            y <- f x.Server;
            return {Server = y.Server, Client = x.Client; y.Client}
    in
        @@mkMonad [t] {Return = @@r, Bind = @@b}
    end

fun server [a] tx =
    x <- tx;
    return {Server = x, Client = return ()}

fun client x =
    return {Server = (), Client = x}

fun run tx =
    x <- tx;
    return <xml>
      <body onload={x.Client}>
        {x.Server}
      </body>
    </xml>
