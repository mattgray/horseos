open Printf

module Make (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) (CL: V1.CLOCK) = struct
  let create console udpv4 logger_ip logger_port = 
      fun message ->
        let time = CL.time () |> CL.gmtime |> fun t -> sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec in
        let m = sprintf "%s: %s" time message in
        C.log_s console m >>
        S.UDPV4.write udpv4 ~dest_ip:logger_ip ~dest_port:logger_port (Cstruct.of_string m)
end
