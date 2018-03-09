include Prelude.Types

signature Input = sig
    type state
    type message
    val step : message -> state -> state
    val init : state
end

functor Make(M : Input) : sig
    type connection
    (* Server side operations. *)
    val send : M.message -> tunit
    val connect : transaction connection
    (* Client side operations. *)
    val listen : connection -> tunit
    val listenWith : connection -> (message -> state -> tunit) -> tunit
    val value : connection -> signal M.state
end
