open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let start c s =
    let horse_ascii = "welcome to HorseOS 0.01          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /--______      ___\\    /\\  :\n| /   __-  - _/   ------    |  |   \\ \\\n |   -  -   /                | |     \\ )\n |  |   -  |                 | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\n" in

    let log_conn flow =
      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      C.log_s c message in

    let write_welcome flow =
      let welcome_message = Cstruct.of_string horse_ascii in
      S.TCPV4.write flow welcome_message >>= function
        | `Ok () -> C.log_s c "write" >> return ()
        | `Eof -> C.log_s c "write: eof" >> S.TCPV4.close flow
        | `Error _ -> C.log_s c "write: error" >> S.TCPV4.close flow in

    let messages = Lwt_condition.create () in

    let rec read_input flow =
      S.TCPV4.read flow >>=function
        | `Eof -> C.log_s c "read: eof" >> S.TCPV4.close flow
        | `Error _ -> C.log_s c "read: error" >> S.TCPV4.close flow
        | `Ok buf -> return ( Lwt_condition.broadcast messages buf ) >> read_input flow in

    let rec handle_message flow = Lwt_condition.wait messages >>=
      fun message -> S.TCPV4.write flow message  >>= function
        | `Ok () -> C.log_s c "write" >> handle_message flow
        | `Eof -> C.log_s c "write: eof" >> S.TCPV4.close flow
        | `Error _ -> C.log_s c "write: error" >> S.TCPV4.close flow in

    C.log c "HorseOS is starting.";

    S.listen_tcpv4 s ~port:4444 (fun flow ->
      log_conn flow >> write_welcome flow >>
      join [ read_input flow; handle_message flow]
    );

    S.listen s;
end
