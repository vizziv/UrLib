open Prelude

functor Make(M : sig
    con r :: {Unit}
    val fl_r : folder r
end) : sig
    type t
    val sqlp : sql_injectable_prim t
    val mk : $(mapU t M.r)
    val match : a ::: Type -> t -> $(mapU a M.r) -> a
end = struct

open M

type t = int

val sqlp = _

val mk =
    let
        val go =
            @fold [fn done => {Index : int, Mk : $(mapU t done)}]
                  (fn [nm :: Name] [u :: Unit] [done :: {Unit}] [[nm] ~ done]
                      (acc : {Index : int, Mk : $(mapU t done)}) =>
                      {Index = acc.Index + 1,
                       Mk = acc.Mk ++ {nm = acc.Index}})
                  {Index = 0, Mk = {}}
                  fl_r
    in
        go.Mk
    end

fun match [a] x ys =
    let
        val go =
            @foldUR [a] [fn done => {Index : int, Match : t -> a}]
                    (fn [nm :: Name] [done :: {Unit}] [[nm] ~ done]
                        (y : a) (acc : {Index : int, Match : t -> a}) =>
                        {Index = acc.Index + 1,
                         Match =
                          fn x =>
                             if x = acc.Index then y else acc.Match x})
                    {Index = 0, Match = fn x => impossible _LOC_}
                    fl_r ys
    in
        go.Match x
    end

end
