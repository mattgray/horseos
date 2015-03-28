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

  module Session : sig
    type flow = S.TCPV4.flow
    type t = { name : Username.t; flow: flow }
    val name : t -> Username.t
    val of_flow : flow -> t
    val set_name : t -> Username.t -> t
    val close : t -> string -> unit Lwt.t
    val write : t -> string -> unit Lwt.t
    val read : t -> ( string -> unit Lwt.t ) ->  unit Lwt.t
  end = struct
    type flow = S.TCPV4.flow
    type t = { name : Username.t; flow: S.TCPV4.flow }
    let name session = session.name
    let of_flow flow = { name = Username.unknown; flow = flow }
    let set_name session name = { session with name }
    let close session reason =
      S.TCPV4.close session.flow
    let write session message =
      S.TCPV4.write session.flow ( Cstruct.of_string message ) >>= function
        | `Eof -> close session "write: eof"
        | `Error _ -> close session "write: error"
        | `Ok () -> Lwt.return_unit
    let read session message_handler =
      let clean_buf buf = Cstruct.to_string buf |> String.trim |> String.escaped in
      S.TCPV4.read session.flow >>= function
        | `Eof -> close session "read: eof"
        | `Error _ -> close session "read: error"
        | `Ok buf -> match Cstruct.get_uint8 buf 0, Cstruct.get_uint8 buf 1 with
          | 255, 244 -> close session "quit"
          | 255, _ -> return ()
          | _ -> match clean_buf buf with
            | "" -> return ()
            | m -> message_handler m
  end

  let messages = Lwt_condition.create ()

  let users = Hashtbl.create 10

  let clean_buf buf = Cstruct.to_string buf |> String.trim |> String.escaped

  let logger c message = C.log c message
(*
  let log_conn log session =
      let dst, dst_port = S.TCPV4.get_dest session.flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      log message; return ()

  let close_conn log session reason =
      let message = ( Username.to_string session.name ) ^ " left: " ^ reason ^ "\n" in
      log reason;
      Hashtbl.remove users ( Username.to_string session.name );
      Lwt_condition.broadcast messages message;
      S.TCPV4.close session.flow

  let write close_conn session message =
    S.TCPV4.write session.flow ( Cstruct.of_string message ) >>= function
        | `Eof -> close_conn session "write: eof"
        | `Error _ -> close_conn session "write: error"
        | `Ok () -> Lwt.return_unit

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
*)
  let start c s =
    let logger = logger c in
  (*  let log_conn = log_conn logger in
    let close_conn = close_conn logger in
    let write = write close_conn in
    let read = read close_conn in
*)
    let write_welcome session = Session.write session horse_ascii in

    let rec listen_input session =
      let broadcast message = return ( Lwt_condition.broadcast messages ( ( Username.to_string ( Session.name session ) ) ^ ": " ^ message ^ "\n") ) in
      Session.read session broadcast >> listen_input session in

    let rec relay_messages session =
      Lwt_condition.wait messages
      >>= fun message -> Session.write session message
      >> relay_messages session in

    let write_userinfo session =
      let user_message = Hashtbl.fold ( fun u _ s -> s ^ " * " ^ u ^ "\n" ) users "Horses in the stable:\n" in
      Session.write session user_message in

    let main session_initial =
      Session.read session_initial ( fun username ->
        if Hashtbl.mem users username then
          Session.write session_initial ( "There's already a user called " ^ username ^ " please try again.\n" )
          >> Session.close session_initial "bad username"
        else
        (
          let session = Session.set_name session_initial ( Username.of_string username ) in
          Hashtbl.add users username session;
          logger ( username ^ " joined" );
          Lwt_condition.broadcast messages (username ^ " joined\n" );
          write_userinfo session
          >> join [ listen_input session; relay_messages session ]
        )
      ) in

    logger "HorseOS is starting.";

    S.listen_tcpv4 s ~port:4444 (fun flow ->

      (*let session_initial = { flow = flow; name = Username.unknown } in
      log_conn session_initial*)
      let session_initial = Session.of_flow flow in 
      write_welcome session_initial
      >> main session_initial
    );

    S.listen s;
end
