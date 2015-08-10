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
  let send console time message =
    C.log_s console (sprintf "%f: %s" time message)
end

module Log_to_syslog_udp (U: V1_LWT.UDPV4) (CL: V1.CLOCK) = struct
  open CL
  type config = {
    udp: U.t;
    ip: Ipaddr.V4.t;
    port: int;
  }
  type t = { udp: U.t; ip: Ipaddr.V4.t; port: int }
  let create (config: config) = {
      udp = config.udp;
      ip = config.ip;
      port = config.port
  }
  let to_rfc3339_string (t : CL.tm) =
    sprintf "%04i-%02i-%02iT%02i:%02i:%02iZ"
      (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  let send t time message =
    let syslog_message =
      sprintf "<34>1 %s giddyup.horse horseos - - [TOKEN@41058] %s"
        (to_rfc3339_string (CL.gmtime time)) message in
    U.write
      t.udp ~dest_ip:t.ip ~dest_port:t.port (Cstruct.of_string syslog_message)
end

module Make (CL: V1.CLOCK) = struct
  open CL

  let build_destination
    (type a)
    (module L : Log_destination with type config = a) config =
      (module struct
        module Log_destination = L
        let this = L.create config
      end : Log_destination_instance)

  let create loggers =
    fun message ->
      let time = CL.time () in
      let f = fun (module I : Log_destination_instance) ->
        I.Log_destination.send I.this time message in
      Lwt_list.iter_p f loggers
end
