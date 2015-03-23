open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    
  let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /\\______      ___\\    /\\  :\n| /   __-@@\\_/   ------    |  |   \\ \\\n |   -  -   \\               | |     \\ )\n |  |   -  | \\              | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: "

  module Username = struct
    type t = | Known of string | Unknown
    let unknown = Unknown
    let of_string s = Known s
    let to_string u =
      match u with
        | Unknown -> "(unknown)"
        | Known s -> s
  end

  type session = { name : Username.t; flow : S.TCPV4.flow }

  let messages = Lwt_condition.create ()

  let users = Hashtbl.create 10

  let clean_buf buf = Cstruct.to_string buf |> String.trim |> String.escaped

  let log_conn c session =
      let dst, dst_port = S.TCPV4.get_dest session.flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      C.log_s c message

  let close_conn c session reason =
      let message = ( Username.to_string session.name ) ^ " left: " ^ reason ^ "\n" in
      C.log c reason;
      Hashtbl.remove users ( Username.to_string session.name );
      Lwt_condition.broadcast messages message;
      S.TCPV4.close session.flow

  let write close_conn session message =
    S.TCPV4.write session.flow ( Cstruct.of_string message ) >>= function
        | `Eof -> close_conn session "write: eof"
        | `Error _ -> close_conn session "write: error"
        | `Ok () -> return ()

  let read close_conn session with_message =
    S.TCPV4.read session.flow >>= function
      | `Eof -> close_conn session "read: eof"
      | `Error _ -> close_conn session "read: error"
      | `Ok buf -> match Cstruct.get_uint8 buf 0, Cstruct.get_uint8 buf 1 with
        | 255, 244 -> close_conn session "quit"
        | 255, _ -> return ()
        | _ -> match clean_buf buf with
          | "" -> return ()
          | m -> with_message m

  let start c s =
    let log_conn = log_conn c in
    let close_conn = close_conn c in
    let write = write close_conn in
    let read = read close_conn in

    let write_welcome session = write session horse_ascii in

    let rec listen_input session =
      let broadcast message = return ( Lwt_condition.broadcast messages ( ( Username.to_string session.name ) ^ ": " ^ message ^ "\n") ) in
      read session broadcast >> listen_input session in

    let rec relay_messages session =
      Lwt_condition.wait messages
      >>= fun message -> write session message
      >> relay_messages session in

    let write_userinfo session =
      let user_message = Hashtbl.fold ( fun u _ s -> s ^ " * " ^ u ^ "\n" ) users "Horses in the stable:\n" in
      write session user_message in

    let main session_initial =
      read session_initial ( fun username ->
        if Hashtbl.mem users username then
          write session_initial ( "There's already a user called " ^ username ^ " please try again.\n" )
          >> close_conn session_initial "bad username"
        else
        (
          let session = { name = Username.of_string username; flow = session_initial.flow } in
          Hashtbl.add users username session;
          C.log c ( username ^ " joined" );
          Lwt_condition.broadcast messages (username ^ " joined\n" );
          write_userinfo session
          >> join [ listen_input session; relay_messages session ]
        )
      ) in

    C.log c "HorseOS is starting.";

    S.listen_tcpv4 s ~port:4444 (fun flow ->
      let session_initial = { flow = flow; name = Username.unknown } in
      log_conn session_initial 
      >> write_welcome session_initial
      >> main session_initial
    );

    S.listen s;
end
