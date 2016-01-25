open Prelude

fun withSources [ts] (fl : folder ts)
                (inits : $ts)
                (sgl : {Get : $(map signal ts),
                        Set : $(map (fn t => t -> tunit) ts)}
                       -> signal xbody)
    : xbody =
    let
        val mkSrcs =
            @Monad.mapR _ [ident] [source]
                        (fn [nm ::_] [t ::_] => source)
                        fl inits
        val gets = @mp [source] [signal] @@signal fl
        val sets = @mp [source] [fn t => t -> tunit] @@set fl
    in
        <xml>
          <active code={srcs <- mkSrcs;
                        return <xml>
                          <dyn signal={sgl {Get = gets srcs,
                                            Set = sets srcs}}/>
                        </xml>}/>
        </xml>
    end

fun submitButtons [r] (fl : folder r) rows =
    let
        fun btn hide {Value = v, Onclick = oc} = <xml>
          <button value={v} onclick={fn _ => hide; oc}/>
        </xml>
    in
        withSources {Show = True}
                    (fn srcs =>
                        show <- srcs.Get.Show;
                        return (if show then
                                    @mapUX [_] [body]
                                           (fn [nm ::_] [rest ::_] [_~_] =>
                                               btn (srcs.Set.Show False))
                                           fl rows
                                else
                                    <xml></xml>))
    end

fun submitButton row : xbody = submitButtons {Button = row}
