open Prelude

structure Ureq = UserRequest.Make(struct
    con handlers = [A = (int, int), B = (int, int)]
    type group = int
    type member = int
    fun mkCont _ ask =
        {A = fn foo =>
                case foo of
                    ({Response = n, Member = m} :: []) =>
                    debug (Misc.plural n "object");
                    ask {Members = Some (0 :: []), Request = (make [#B] n)}
                  | _ => return (),
         B = fn foo =>
                case foo of
                    ({Response = n, Member = m} :: []) =>
                    debug (Misc.plural n "thingy");
                    ask {Members = Some (0 :: []), Request = (make [#A] n)}
                  | _ => return ()}
end)

val start = Ureq.ask {Group = 0,
                      Members = Some (0 :: []),
                      Request = (make [#A] 9001)}

fun test _ =
    srvqSrc <- source None;
    let
        fun render srvq =
            case srvq of
                None => <xml>Nothing to do.</xml>
              | Some srv =>
                cases {A = fn sr =>
                              <xml>
                                A {[sr.Request]}:
                                {Ui.button {Value = "Click me!",
                                            Onclick =
                                            sr.Submit (sr.Request + 5)}}
                              </xml>,
                       B = fn sr =>
                              <xml>
                                B {[sr.Request]}:
                                {Ui.button {Value = "Click me!",
                                            Onclick =
                                            sr.Submit (sr.Request - 3)}}
                              </xml>}
                      srv
    in
        return <xml>
          <body>
            <h1>ClientRequest Test</h1>
            <dyn signal={Monad.mp render (signal srvqSrc)}/>
            {Ui.button {Value = "Start listening",
                        Onclick =
                        Ureq.subscribeSource {Group = 0, Member = 0} srvqSrc;
                        rpc start}}
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
