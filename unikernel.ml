open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    
  let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /\\______      ___\\    /\\  :\n| /   __-@@\\_/   ------    |  |   \\ \\\n |   -  -   \\               | |     \\ )\n |  |   -  | \\              | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: "

  type session = { name : string; flow : S.TCPV4.flow }

  let messages = Lwt_condition.create ()

  let users = Hashtbl.create 10

  let clean_buf buf = Cstruct.to_string buf |> String.trim |> String.escaped

  let log_conn c flow =
      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      C.log_s c message

  let close_conn c flow username reason =
      let message = username ^ " left: " ^ reason ^ "\n" in
      C.log c reason;
      Hashtbl.remove users username;
      Lwt_condition.broadcast messages message;
      S.TCPV4.close flow
 
  let start c s =
    let log_conn = log_conn c in
    let close_conn = close_conn c in

    let write_welcome flow =
      let welcome_message = Cstruct.of_string horse_ascii in
      S.TCPV4.write flow welcome_message >>= function
        | `Eof -> close_conn flow "(unknown)" "write: eof"
        | `Error _ -> close_conn flow "(unknown)" "write: error"
        | `Ok () -> return () in

    let rec listen_input session =
      S.TCPV4.read session.flow >>= function
        | `Eof -> close_conn session.flow session.name "read: eof"
        | `Error _ -> close_conn session.flow session.name "read: error"
        | `Ok buf -> let message = ( clean_buf buf ) in
          if Cstruct.get_uint8 buf 0 != 255 then Lwt_condition.broadcast messages ( session.name ^ ": " ^ message ^ "\n");
          listen_input session in

    let rec read_input username flow =
      S.TCPV4.read flow >>= function
        | `Eof -> close_conn flow username "read: eof"
        | `Error _ -> close_conn flow username "read: error"
        | `Ok buf -> let message = ( clean_buf buf ) in
          if Cstruct.get_uint8 buf 0 != 255 then Lwt_condition.broadcast messages ( username ^ ": " ^ message ^ "\n");
          read_input username flow in

    let rec handle_message username flow = Lwt_condition.wait messages >>=
      fun message -> S.TCPV4.write flow ( Cstruct.of_string message )  >>= function
        | `Eof -> close_conn flow username "read: eof"
        | `Error _ -> close_conn flow username "read: error"
        | `Ok () -> handle_message username flow in

    let write_userinfo username flow =
      let user_message = Hashtbl.fold ( fun u _ s -> s ^ " * " ^ u ^ "\n" ) users "Horses in the stable:\n" in
      S.TCPV4.write flow ( Cstruct.of_string user_message )  >>= function
        | `Eof -> close_conn flow username "read: eof"
        | `Error _ -> close_conn flow username "read: error"
        | `Ok () ->  return () in

    let main flow =
      S.TCPV4.read flow >>= function
        | `Eof -> close_conn flow "(unknown)" "read: eof"
        | `Error _ -> close_conn flow "(unknown)" "read: error"
        | `Ok buf ->
          let username = ( clean_buf buf ) in
          if Cstruct.get_uint8 buf 0 == 255 then close_conn flow "(unknown)" "we dont negotiate telnet options" else
          (
            if Hashtbl.mem users username then
              S.TCPV4.write flow ( Cstruct.of_string ( "There's already a user called " ^ username ^ " please try again.\n" ) )
                >> close_conn flow "(unknown)" "bad username"
            else
            (
              let session = { name = username; flow = flow } in
              Hashtbl.add users username session;
              C.log c ( username ^ " joined" );
              Lwt_condition.broadcast messages (username ^ " joined\n" );
              write_userinfo username flow >>
              join [ listen_input session; handle_message username flow ]
            )
          ) in

    C.log c "HorseOS is starting.";

    S.listen_tcpv4 s ~port:4444 (fun flow ->
      log_conn flow >> write_welcome flow >> main flow
    );

    S.listen s;
end
