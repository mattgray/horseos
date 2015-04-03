open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct
    
  let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /\\______      ___\\    /\\  :\n| /   __-@@\\_/   ------    |  |   \\ \\\n |   -  -   \\               | |     \\ )\n |  |   -  | \\              | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: "

  module Session : sig
    type t

    val of_flow : S.TCPV4.flow -> t

    val on_close : t -> ( string -> unit ) -> t

    val close : t -> string -> unit Lwt.t

    val write : t -> string -> unit Lwt.t

    val read : t -> ( string -> unit Lwt.t ) ->  unit Lwt.t

  end = struct
    
    type t = { flow: S.TCPV4.flow; on_close: ( string -> unit ) }

    let of_flow f = { flow = f; on_close = fun _ -> () }

    let on_close session closer = { session with on_close = closer }

    let close session reason =
      session.on_close reason;
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

  let start c s =
    let log message = C.log c message in

    let write_welcome session = Session.write session horse_ascii in

    let rec listen_input session username =
      let broadcast message = return ( Lwt_condition.broadcast messages ( username ^ ": " ^ message ^ "\n") )  in
      Session.read session broadcast >> listen_input session username in

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
          let on_close reason =
              let message = ( username ^ " has quit (" ^ reason ^  ")\n" ) in
                log message;
                Lwt_condition.broadcast messages ( username ^ " has quit (" ^ reason ^  ")\n" );
                Hashtbl.remove users username
              in
          let session = Session.on_close session_initial on_close in
          Hashtbl.add users username session;
          log ( username ^ " joined" );
          Lwt_condition.broadcast messages (username ^ " joined\n" );
          write_userinfo session
          >> join [ listen_input session username; relay_messages session ]
        )
      ) in

    log "HorseOS is starting.";

    S.listen_tcpv4 s ~port:4444 (fun flow ->

      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      log message;

      let session_initial = Session.of_flow flow in 
      write_welcome session_initial
      >> main session_initial
    );

    S.listen s;
end
