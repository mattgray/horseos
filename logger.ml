module Make (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
  let create console udpv4 logger_ip logger_port = 
      fun message -> C.log_s console message >>
      S.UDPV4.write udpv4 ~dest_ip:logger_ip ~dest_port:logger_port (Cstruct.of_string message)
end
