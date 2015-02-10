open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let start c s = 
    C.log c "HorseOS is starting.";
    (* start listening on tcp:4444 (randomly chosen nice port) *)
    S.listen_tcpv4 s ~port:4444 (fun flow ->
        let dst, dst_port = S.TCPV4.get_dest flow in
        C.log_s c "Got a connection"
        >>= fun () ->
        S.TCPV4.close flow
      );
    S.listen s
end
