open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let start c s =
    let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /\\______      ___\\    /\\  :\n| /   __-@@\\_/   ------    |  |   \\ \\\n |   -  -   \\               | |     \\ )\n |  |   -  | \\              | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: " in

    let log_conn flow =
      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      C.log_s c message in

    let messages = Lwt_condition.create () in

    let close_conn flow username reason =
      let message = username ^ " left: " ^ reason ^ "\n" in
        C.log_s c reason >>
        return ( Lwt_condition.broadcast messages message ) >>
        S.TCPV4.close flow in

    let write_welcome flow =
      let welcome_message = Cstruct.of_string horse_ascii in
      S.TCPV4.write flow welcome_message >>= function
        | `Eof -> close_conn flow "(unknown)" "write: eof"
        | `Error _ -> close_conn flow "(unknown)" "write: error"
        | `Ok () -> return () in

    let rec read_input username flow =
      S.TCPV4.read flow >>= function
        | `Eof -> close_conn flow username "read: eof"
        | `Error _ -> close_conn flow username "read: error"
        | `Ok buf -> let message = Cstruct.to_string buf in
          return ( Lwt_condition.broadcast messages ( username ^ ": " ^ message ) ) >>
          read_input username flow in

    let rec handle_message username flow = Lwt_condition.wait messages >>=
      fun message -> S.TCPV4.write flow ( Cstruct.of_string message )  >>= function
        | `Eof -> close_conn flow username "read: eof"
        | `Error _ -> close_conn flow username "read: error"
        | `Ok () -> handle_message username flow in

    let main flow =
      S.TCPV4.read flow >>= function
        | `Eof -> close_conn flow "(unknown)" "read: eof"
        | `Error _ -> close_conn flow "(unknown)" "read: error"
        | `Ok buf -> let username = String.trim ( Cstruct.to_string buf ) in
          C.log_s c ( username ^ " joined" ) >>
          return ( Lwt_condition.broadcast messages (username ^ " joined\n" ) ) >>
          join [ read_input username flow; handle_message username flow ] in

    C.log c "HorseOS is starting.";

    S.listen_tcpv4 s ~port:4444 (fun flow ->
      log_conn flow >> write_welcome flow >> main flow
    );

    S.listen s;
end
