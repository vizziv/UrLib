structure Ureq = UserRequest.Make(struct
    con handlers = [A = (int, int), B = (int, int)]
    type group = int
    type member = int
    fun mkCont ask =
        {A = fn foo =>
                case foo of
                    ({Response = n, Member = m} :: _) =>
                    debug (Misc.plural n "object");
                    ask {Members = 0 :: [], Request = (make [#B] n)}
                  | _ => return (),
         B = fn foo =>
                case foo of
                    ({Response = n, Member = m} :: _) =>
                    debug (Misc.plural n "thingy");
                    ask {Members = 0 :: [], Request = (make [#A] n)}
                  | _ => return ()}
end)

val start = Ureq.ask {Group = 0,
                      Members = 0 :: [],
                      Request = (make [#A] 9001)}

fun test _ =
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
                          Onclick =
                          Ureq.listen {Group = 0, Member = 0} (handlers answerqSrc);
                          rpc start}}
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
