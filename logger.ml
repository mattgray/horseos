open Printf

module Make (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) (CL: V1.CLOCK) = struct
  open CL

  let to_rfc3339_string (t : CL.tm) = sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  let get_time () = CL.time () |> CL.gmtime

  let log_console console time message = C.log_s console (sprintf "%s: %s" time message)

  let log_syslog udpv4 ip port time message = 
    let syslog_message = sprintf "<34>1 %s giddyup.horse horseos - - [TOKEN@41058] %s" time message in
    S.UDPV4.write udpv4 ~dest_ip:ip ~dest_port:port (Cstruct.of_string syslog_message)

  let create console udpv4 ip port =
      fun message ->
        let time = get_time () |> to_rfc3339_string in
        log_console console time message >>
        log_syslog udpv4 ip port time message
end
