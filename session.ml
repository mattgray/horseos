open V1_LWT

type t = { flow: STACKV4.TCPV4.flow; on_close: ( string -> unit ) }

let of_flow f = { flow = f; on_close = fun _ -> () }

let on_close session closer = { session with on_close = closer }

let close session reason =
      session.on_close reason;
      STACKV4.TCPV4.close session.flow

let write session message =
  STACKV4.TCPV4.write session.flow ( Cstruct.of_string message ) >>= function
    | `Eof -> close session "write: eof"
    | `Error _ -> close session "write: error"
    | `Ok () -> Lwt.return_unit

let read session message_handler =
  let clean_buf buf = Cstruct.to_string buf |> String.trim |> String.escaped in
    STACKV4.TCPV4.read session.flow >>= function
      | `Eof -> close session "read: eof"
      | `Error _ -> close session "read: error"
      | `Ok buf -> match Cstruct.get_uint8 buf 0, Cstruct.get_uint8 buf 1 with
        | 255, 244 -> close session "quit"
        | 255, _ -> return ()
        | _ -> match clean_buf buf with
          | "" -> return ()
          | m -> message_handler m
