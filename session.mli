type t

val of_flow : V1_LWT.STACKV4.TCPV4.flow -> t

val on_close : t -> ( string -> unit ) -> t

val close : t -> string -> unit Lwt.t

val write : t -> string -> unit Lwt.t

val read : t -> ( string -> unit Lwt.t ) ->  unit Lwt.t
