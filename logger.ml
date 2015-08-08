open Printf

module type Log_destination = sig
  type config
  type t
  val create: config -> t
  val send: t -> float -> bytes -> unit Lwt.t
end

module type Log_destination_instance = sig
  module Log_destination : Log_destination
  val this : Log_destination.t
end

module Log_to_console (C: V1_LWT.CONSOLE) = struct
  type config = C.t
  type t = C.t
  let create console = console
  let send console time message = C.log_s console message
end

module Make (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) (CL: V1.CLOCK) = struct
  open CL

  let build_destination (type a) (module L : Log_destination with type config = a) config =
    (module struct
      module Log_destination = L
      let this = L.create config
    end : Log_destination_instance)

  let to_rfc3339_string (t : CL.tm) = sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ" (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec

  let log_console console time message = C.log_s console (sprintf "%s: %s" time message)

  let log_syslog udpv4 ip port time message = 
    let syslog_message = sprintf "<34>1 %s giddyup.horse horseos - - [TOKEN@41058] %s" time message in
    S.UDPV4.write udpv4 ~dest_ip:ip ~dest_port:port (Cstruct.of_string syslog_message)

  let create console udpv4 ip port =
      fun message ->
        let time = CL.time () in
        let console_dest = build_destination (module Log_to_console(C)) console in
        let module I = ((val console_dest) : Log_destination_instance) in
        I.Log_destination.send I.this time message >>
          log_syslog udpv4 ip port (to_rfc3339_string (CL.gmtime time)) message
end
