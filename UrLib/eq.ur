con t = K ==>
 fn a b => tf ::: (K -> Type) -> {Left : tf a -> tf b, Right : tf b -> tf a}

val refl = K ==> fn [a] [tf] => {Left = fn x => x, Right = fn x => x}

val symm = K ==>
 fn [a] [b] (pf : t a b) [tf] => {Left = pf.Right, Right = pf.Left}

val trans = K ==>
 fn [a] [b] [c] (pf1 : t a b) (pf2 : t b c) [tf] =>
    {Left = compose pf2.Left pf1.Left,
     Right = compose pf1.Right pf2.Right}

val cast = K ==> fn [a] [b] (pf : t a b) [tf ::_] => pf.Left
