fun buttons' rows =
    srcShouldShow <- source True;
    let
        val sgl =
            shouldShow <- signal srcShouldShow;
            if shouldShow then
                return (List.mapX (fn {Value = value, Onclick = onclick} =>
                                      <xml>
                                        <button value={value}
                                        onclick={fn _ =>
                                                    set srcShouldShow False;
                                                    onclick}/>
                                      </xml>)
                                  rows)
            else
                return <xml></xml>
    in
        return <xml><dyn signal={sgl}/></xml>
    end

fun buttons rows : xbody = <xml><active code={buttons' rows}/></xml>

fun button row : xbody = buttons (row :: [])
