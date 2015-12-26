(* TODO: investigate behavior with @ and switching constraint with [r]. *)
fun submitButtons' [ctx] [r] [ctx ~ body] (fl : folder r) rows
    : transaction (xml (ctx ++ body) [] []) =
    srcShouldShow <- source True;
    let
        val sgl =
            shouldShow <- signal srcShouldShow;
            if shouldShow then
                return (@mapUX [_] [ctx ++ body]
                               (fn [nm ::_] [rest ::_] [_~_]
                                   {Value = value, Onclick = onclick} =>
                                   <xml>
                                     <button
                                     value={value}
                                     onclick={fn _ =>
                                                 set srcShouldShow False;
                                                 onclick}/>
                                   </xml>)
                               fl
                              rows)
            else
                return <xml></xml>
    in
        return <xml><dyn signal={sgl}/></xml>
    end

fun submitButtons [r] (fl : folder r) rows : xbody =
    <xml><active code={@submitButtons' ! fl rows}/></xml>

fun submitButton row : xbody = submitButtons {Button = row}
