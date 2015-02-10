open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let start c s = 
    C.log c "HorseOS is starting.";
    (* start listening on tcp:4444 (randomly chosen nice port) *)
    S.listen_tcpv4 s ~port:4444 (fun flow ->
        let dst, dst_port = S.TCPV4.get_dest flow in
        let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
        C.log c message;
        S.TCPV4.read flow >>= function
        | `Ok b -> C.log_s c ( "read some data: " ^ (Cstruct.to_string b) ) >>= fun () -> S.TCPV4.close flow
        | `Eof -> C.log_s c "read: eof"
        | `Error _ -> C.log_s c "read: error"
      );
    S.listen s
end
