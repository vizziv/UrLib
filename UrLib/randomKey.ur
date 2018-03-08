structure T = Newtype.Make(struct
    type t = int
    con interface = Newtype.sqlp ++ Newtype.random
    val t = _
end)

open T

val rng = t.Rng
val sqlp = t.Sqlp
