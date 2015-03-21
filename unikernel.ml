open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    
  let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /\\______      ___\\    /\\  :\n| /   __-@@\\_/   ------    |  |   \\ \\\n |   -  -   \\               | |     \\ )\n |  |   -  | \\              | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: "

  module Username : sig
    type username
    val unknown : username
    val of_string : string -> username
    val to_string : username -> string
  end = struct
    type username = | Known of string | Unknown
    let unknown = Unknown
    let of_string s = Known s
    let to_string u =
      match u with
        | Unknown -> "(unknown)"
        | Known s -> s
  end

  type session = { name : Username.username; flow : S.TCPV4.flow }

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
 
  let start c s =
    let log_conn = log_conn c in
    let close_conn = close_conn c in

    let write_welcome session =
      let welcome_message = Cstruct.of_string horse_ascii in
      S.TCPV4.write session.flow welcome_message >>= function
        | `Eof -> close_conn session "write: eof"
        | `Error _ -> close_conn session "write: error"
        | `Ok () -> return () in

    let rec listen_input session =
      S.TCPV4.read session.flow >>= function
        | `Eof -> close_conn session "read: eof"
        | `Error _ -> close_conn session "read: error"
        | `Ok buf -> let message = ( clean_buf buf ) in
          if Cstruct.get_uint8 buf 0 != 255 then Lwt_condition.broadcast messages ( ( Username.to_string session.name ) ^ ": " ^ message ^ "\n");
          listen_input session in

    let rec broadcast_chats session = Lwt_condition.wait messages >>=
      fun message -> S.TCPV4.write session.flow ( Cstruct.of_string message )  >>= function
        | `Eof -> close_conn session "read: eof"
        | `Error _ -> close_conn session "read: error"
        | `Ok () -> broadcast_chats session in

    let write_userinfo session =
      let user_message = Hashtbl.fold ( fun u _ s -> s ^ " * " ^ u ^ "\n" ) users "Horses in the stable:\n" in
      S.TCPV4.write session.flow ( Cstruct.of_string user_message ) >>= function
        | `Eof -> close_conn session "read: eof"
        | `Error _ -> close_conn session "read: error"
        | `Ok () ->  return () in

    let main session_initial =
      S.TCPV4.read session_initial.flow >>= function
        | `Eof -> close_conn session_initial "read: eof"
        | `Error _ -> close_conn session_initial "read: error"
        | `Ok buf ->
          let username = ( clean_buf buf ) in
          if Cstruct.get_uint8 buf 0 == 255 then close_conn session_initial "we dont negotiate telnet options" else
          (
            if Hashtbl.mem users username then
              S.TCPV4.write session_initial.flow ( Cstruct.of_string ( "There's already a user called " ^ username ^ " please try again.\n" ) )
                >> close_conn session_initial "bad username"
            else
            (
              let session = { name = Username.of_string username; flow = session_initial.flow } in
              Hashtbl.add users username session;
              C.log c ( username ^ " joined" );
              Lwt_condition.broadcast messages (username ^ " joined\n" );
              write_userinfo session >>
              join [ listen_input session; broadcast_chats session ]
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
