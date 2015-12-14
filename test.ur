structure CR = ClientRequest.Make(struct
    fun mkCont ask =
        {A = fn n => debug (Misc.plural n "object"); ask (make [#B] n),
         B = fn n => debug (Misc.plural n "thingy"); ask (make [#A] n)}
end)

fun test _ =
    CR.ask (make [#A] 9001);
    let
        fun sgl answerqSrc =
            answerq <- signal answerqSrc;
            case answerq of
                None => return <xml>Nothing to do</xml>
              | Some answer =>
                return <xml>
                  Submit: {Ui.button {Value = "Click me!", Onclick = answer}}
                </xml>
        fun handlers answerqSrc =
            {A = fn answer n => set answerqSrc (Some (answer (n+5))),
             B = fn answer n => answer (n-2)}
        val mkForm =
            answerqSrc <- source None;
            return <xml>
              <dyn signal={sgl answerqSrc}/>
              {Ui.button {Value = "Start listening",
                          Onclick = CR.listen (handlers answerqSrc)}}
            </xml>
    in
        return <xml>
          <body>
            <h1>ClientRequest Test</h1>
            <active code={mkForm}/>
          </body>
        </xml>
    end

val main : transaction page =
    return <xml>
      <body>
        <form>
          <submit value="Make request" action={test}/>
        </form>
      </body>
    </xml>
