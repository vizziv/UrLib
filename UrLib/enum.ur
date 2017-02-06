open Prelude

functor Make(M : sig
    con r :: {Unit}
    val fl : folder r
    val label : $(mapU string r)
end) : sig
    type t
    val eq : eq t
    val read : read t
    val show : show t
    val sqlp : sql_injectable_prim t
    val mk : $(mapU t M.r)
    val match : a ::: Type -> t -> $(mapU a M.r) -> a
end = struct

open M

type t = int

val mk =
    let
        val go =
            @fold [fn done => {Index : int, Mk : $(mapU t done)}]
                  (fn [nm :: Name] [u :: Unit] [done :: {Unit}] [[nm] ~ done]
                      (acc : {Index : int, Mk : $(mapU t done)}) =>
                      {Index = acc.Index + 1,
                       Mk = acc.Mk ++ {nm = acc.Index}})
                  {Index = 0, Mk = {}}
                  fl
    in
        go.Mk
    end

fun match [a] x ys =
    let
        val go =
            @foldUR2 [t] [a] [fn _ => t -> a]
                     (fn [nm :: Name] [done :: {Unit}] [[nm] ~ done]
                         (v : t) (y : a) (acc : t -> a)
                         (x : t) =>
                         if x = v then y else acc x)
                     (fn x => impossible _LOC_)
                     fl mk ys
    in
        go x
    end

val eq = _

val read =
    let
        val go =
            @foldUR2 [string] [t] [fn _ => string -> option t]
                     (fn [nm :: Name] [done :: {Unit}] [[nm] ~ done]
                         (l : string) (v : t) (acc : string -> option t)
                         (s : string) =>
                         if s = l then Some v else acc s)
                     (fn x => None)
                     fl label mk
    in
        mkRead' go "Enum.Make.t"
    end

val show = mkShow (fn x => match x label)

val sqlp = _

end
