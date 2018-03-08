open Prelude

con t = transaction

fun mk [a] (f : int -> a) = Monad.mp f rand

val gen = fn [a] => identity

val int = rand
