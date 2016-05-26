con t = K ==>
 fn a b =>
    tf ::: (K -> Type) -> {Left : tf a -> tf b, Right : tf b -> tf a}
val refl = K ==> fn [a] [tf] => {Left = fn x => x, Right = fn x => x}
fun symm [K] [a] [b] (pf : t a b) [tf] = {Left = pf.Right, Right = pf.Left}
fun trans [K] [a] [b] [c] (pf1 : t a b) (pf2 : t b c) [tf] =
    {Left = compose pf2.Left pf1.Left, Right = compose pf1.Right pf2.Right}
fun cast [K] [a] [b] (pf : t a b) [tf ::_] = pf.Left

con t0 a b =
    tf ::: (Type -> Type) -> {Left : tf a -> tf b, Right : tf b -> tf a}
fun mk0 [a] [b] pf = pf
val refl0 = fn [a] [tf] => {Left = fn x => x, Right = fn x => x}
fun symm0 [a] [b] (pf : t0 a b) [tf] = {Left = pf.Right, Right = pf.Left}
fun trans0 [a] [b] [c] (pf1 : t0 a b) (pf2 : t0 b c) [tf] =
    {Left = compose pf2.Left pf1.Left, Right = compose pf1.Right pf2.Right}
fun cast0 [a] [b] (pf : t0 a b) [tf ::_] = pf.Left

con t1 a b =
    tf ::: ({Type} -> Type) -> {Left : tf a -> tf b, Right : tf b -> tf a}
fun mk1 [a] [b] pf = pf
val refl1 = fn [a] [tf] => {Left = fn x => x, Right = fn x => x}
fun symm1 [a] [b] (pf : t1 a b) [tf] = {Left = pf.Right, Right = pf.Left}
fun trans1 [a] [b] [c] (pf1 : t1 a b) (pf2 : t1 b c) [tf] =
    {Left = compose pf2.Left pf1.Left, Right = compose pf1.Right pf2.Right}
fun cast1 [a] [b] (pf : t1 a b) [tf ::_] = pf.Left
