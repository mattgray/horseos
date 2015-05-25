open Printf

module Make (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) (CL: V1.CLOCK) = struct
  let create console udpv4 logger_ip logger_port =
      let get_time_string = let open CL in CL.time () |> CL.gmtime |> fun t -> sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
      let log_console time message = C.log_s console (sprintf "%s: %s" time message) in
      let log_syslog time message =
        let syslog_message = sprintf "<34>1 %s giddyup.horse horseos - - [TOKEN@41058] %s" time message in
          S.UDPV4.write udpv4 ~dest_ip:logger_ip ~dest_port:logger_port (Cstruct.of_string syslog_message) >> C.log_s console syslog_message in
      fun message ->
        log_console get_time_string message >>
        log_syslog get_time_string message
end
