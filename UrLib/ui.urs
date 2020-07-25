include Prelude.Types

(* Create [source]s and use them in dynamic XML. *)
val withSources :
    ts ::: {Type} -> folder ts ->
    $ts ->
    ({Get : $(map signal ts),
      Set : $(map (fn t => t -> tunit) ts),
      Source : $(map source ts)}
     -> signal xbody)
    -> xbody

(* A series of buttons that disappears after clicking any of them. *)
val submitButtons :
    r ::: {Unit} -> folder r ->
    $(mapU {Value : string, Onclick : tunit} r)
    -> xbody

(* A single button that disappears after clicking it. *)
val submitButton : {Value : string, Onclick : tunit} -> xbody
