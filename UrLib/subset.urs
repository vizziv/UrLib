class t :: {Type} -> {Type} -> Type
val mk : keep ::: {Type} -> drop ::: {Type} -> [keep ~ drop]
         => t (keep ++ drop) keep
(* val mp : fields ::: {Type} -> keep ::: {Type} -> f ::: (Type -> Type) *)
(*          -> t fields keep -> t (map f fields) (map f keep) *)
val projs : fields ::: {Type} -> keep ::: {Type} -> t fields keep
            -> $fields -> $keep
