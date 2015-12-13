fun buttons' rows =
    srcShouldShow <- source True;
    let
        val sgl =
            shouldShow <- signal srcShouldShow;
            if shouldShow
            then return (List.mapX (fn btn => <xml>
              <button value={btn.Value}
                      onclick={fn _ => set srcShouldShow False; btn.Onclick}/>
            </xml>) rows)
            else return <xml></xml>
    in
        return <xml><dyn signal={sgl}/></xml>
    end

fun buttons rows = <xml><active code={buttons' rows}/></xml>

fun button row = buttons (row :: [])
