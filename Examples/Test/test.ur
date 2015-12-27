open Prelude

structure Ureq = UserRequest.Make(struct
    con handlers = [A = (int, int), B = (int, int)]
    type group = int
    type member = int
    fun cont _ ask =
        {A = fn foo =>
                case foo of
                    ({Response = n, Member = m} :: []) =>
                    debug (Misc.plural n "object");
                    ask (make [#B] ({Member = 0, Request = n} :: []))
                  | _ => return (),
         B = fn foo =>
                case foo of
                    ({Response = n, Member = m} :: []) =>
                    debug (Misc.plural n "thingy");
                    ask (make [#A] ({Member = 0, Request = n} :: []))
                  | _ => return ()}
end)

val start = Ureq.ask 0 (make [#A] ({Member = 0, Request = 9001} :: []))

fun test _ =
    connection <- Ureq.connect {Group = 0, Member = 0};
    let
        fun render srvq =
            case srvq of
                None => <xml>Nothing to do.</xml>
              | Some srv =>
                cases {A = fn sr =>
                              <xml>
                                A {[sr.Request]}:
                                {Ui.submitButton
                                     {Value = "Click me!",
                                      Onclick = sr.Submit (sr.Request + 5)}}
                              </xml>,
                       B = fn sr =>
                              <xml>
                                B {[sr.Request]}:
                                {Ui.submitButton
                                     {Value = "Click me!",
                                      Onclick = sr.Submit (sr.Request - 3)}}
                              </xml>}
                      srv
    in
        return <xml>
          <body>
            <h1>ClientRequest Test</h1>
            <dyn signal={Monad.mp render (Ureq.value connection)}/>
            {Ui.submitButton {Value = "Start listening",
                              Onclick = Ureq.listen connection; rpc start}}
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
