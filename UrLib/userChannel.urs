include Prelude.Types

con t :: Type -> Type
con connection :: Type -> Type

(* Server side operations. *)
val send : a ::: Type -> t a -> a -> tunit
val connect : a ::: Type -> t a -> connection a

(* Client side operations. *)
val recv : a ::: Type -> connection a -> transaction a
val spawnListener : a ::: Type -> connection a -> (a -> tunit) -> tunit

functor Make(M : sig type a end) : sig
    val t : t M.a
end
