con t = K ==>
 fn a b =>
    tf ::: (K -> Type) -> {Left : tf a -> tf b, Right : tf b -> tf a}

val refl = K ==> fn [a] [tf] => {Left = fn x => x, Right = fn x => x}

fun symm [K] [a] [b] (pf : t a b) [tf] = {Left = pf.Right, Right = pf.Left}

fun trans [K] [a] [b] [c] (pf1 : t a b) (pf2 : t b c) [tf] =
    {Left = pf1.Left >>> pf2.Left, Right = pf1.Right <<< pf2.Right}

fun cast [K] [a] [b] (pf : t a b) [tf ::_] = pf.Left
