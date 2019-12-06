con t = K ==>
 fn a b =>
    tf ::: (K -> Type) -> {Fwd : tf a -> tf b, Bwd : tf b -> tf a}

val refl = K ==> fn [a] [tf] => {Fwd = fn x => x, Bwd = fn x => x}

fun symm [K] [a] [b] (pf : t a b) [tf] = {Fwd = pf.Bwd, Bwd = pf.Fwd}

fun trans [K] [a] [b] [c] (pf1 : t a b) (pf2 : t b c) [tf] =
    {Fwd = pf1.Fwd >>> pf2.Fwd, Bwd = pf1.Bwd <<< pf2.Bwd}

fun cast [K] [a] [b] (pf : t a b) [tf ::_] = pf.Fwd

fun over
        [K1] [K2] [a1] [b1] [a2] [b2]
        (pf1 : t a1 b1) [tf1 ::_] (pf2 : t a2 b2) [tf2 ::_]
        (f : tf1 b1 -> tf2 b2) =
    pf1.Fwd >>> f >>> pf2.Bwd

fun mp [K1] [K2] [a] [b] [tf ::_] (pf : t a b) [tg] = @@pf [fn a => tg (tf a)]

fun make [nm ::_] [a] [others] [r] [[nm] ~ others] (pf : t _ _) =
    Basis.make [nm] >>> (@@pf [variant]).Fwd

fun makeMap
        [K]
        [nm ::_] [a] [others] [r]
        [[nm] ~ others]
        [tf ::_]
        (pf : t _ _)
        (x : tf a) =
    (@@pf [fn r => variant (map tf r)]).Fwd (Basis.make [nm] x)
