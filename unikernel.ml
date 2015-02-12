open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct


  let callback c flow =
  
    let log_conn c flow =
      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      C.log_s c message in

    let handle_read = function
      | `Ok buf -> C.log_s c ( "read some data: " ^ (Cstruct.to_string buf) )
      | `Eof -> C.log_s c "read: eof"
      | `Error _ -> C.log_s c "read: error" in

    log_conn c flow >> 
    S.TCPV4.read flow >>=
    handle_read >>
    S.TCPV4.close flow

  let start c s = 
    C.log c "HorseOS is starting.";
    S.listen_tcpv4 s ~port:4444 (callback c);
    S.listen s
end
