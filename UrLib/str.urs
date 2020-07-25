(* Does its best to correctly pluralize English words.
   [
       plural 1 "bat" = "1 bat"
       plural 3 "bat" = "3 bats"
       plural 3 "box" = "3 boxes"
       plural 3 "bus" = "3 buses"
       plural 3 "ski" = "3 skis"
       plural 3 "sky" = "3 skies"
   ]
   It's not very fancy.
   [
       plural 3 "pig a-flying" = "3 pig a-flyings"
       plural 3 "" = "3 "
   ]
 *)
val plural : int -> string -> string

(* Formats a list of strings.
   [
       stringList ("a" :: "b" :: "c" :: []) = "[a, b, c]"
   ]
 *)
val stringList : list string -> string

(* Combines show and stringList. *)
val showList : t ::: Type -> show t -> list t -> string
