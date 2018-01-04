include Prelude.Types

val withSources :
    ts ::: {Type} -> folder ts ->
    $ts ->
    ({Get : $(map signal ts),
      Set : $(map (fn t => t -> tunit) ts),
      Source : $(map source ts)}
     -> signal xbody)
    -> xbody

val submitButtons :
    r ::: {Unit} -> folder r ->
    $(mapU {Value : string, Onclick : tunit} r)
    -> xbody

val submitButton : {Value : string, Onclick : tunit} -> xbody
