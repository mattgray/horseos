open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct


  let callback c flow =
  
    let log_conn c flow =
      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      C.log_s c message in
    let handle_write buf = S.TCPV4.write flow buf >>= function
      | `Ok () -> C.log_s c "write" >> S.TCPV4.close flow
      | `Eof -> C.log_s c "write: eof" >> S.TCPV4.close flow
      | `Error _ -> C.log_s c "write: error" >> S.TCPV4.close flow
    in
    let handle_read = S.TCPV4.read flow >>= function
      | `Eof -> C.log_s c "read: eof" >> S.TCPV4.close flow
      | `Error _ -> C.log_s c "read: error" >> S.TCPV4.close flow
      | `Ok buf -> handle_write buf
    in
    log_conn c flow >>
    handle_read

  let start c s = 
    C.log c "HorseOS is starting.";
    S.listen_tcpv4 s ~port:4444 (callback c);
    S.listen s
end
