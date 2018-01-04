open Prelude

signature Input = sig
    con fields :: {Type}
    con ref :: Name
    constraint [ref] ~ fields
    val fl : folder fields
    val sql : $(map sql_injectable fields)
end

functor Make(M : Input) = struct

(* TODO: pulse and cookie expiration. *)

open M

cookie ref : int

table refs : ([ref = int] ++ fields)

fun setRef rr =
    setCookie ref {Value = rr.ref, Expires = None, Secure = True}

fun set xs =
    bind (@Sql.insertRandKeys ! _ fl sql refs xs) setRef

val get =
    rq <- getCookie ref;
    case rq of
        None => return None
      | Some r =>
        xsq <- oneOrNoRows1 (Sql.selectLookup refs {ref = r});
        case xsq of
            None => return None
          | Some xs =>
            bind (Sql.updateRandKeys refs {ref = r}) setRef;
            return (Some xs)

end
