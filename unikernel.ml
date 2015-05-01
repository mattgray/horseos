open Lwt
open Session

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  module Session = Session.Tcp(S)

  module Horse_greeter = struct
    let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /--______      ___\\    /\\  :\n| /   __-  - _/   ------    |  |   \\ \\\n |   -  -   /                | |     \\ )\n |  |   -  |                 | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: "
    let greet session = Session.write session horse_ascii
  end

  let messages = Lwt_condition.create ()

  let users = Hashtbl.create 10

  let start c s =
    let log message = C.log c message in

    let rec listen_input session username =
      let broadcast message = return ( Lwt_condition.broadcast messages ( username ^ ": " ^ message ^ "\n") ) in
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
      Horse_greeter.greet session_initial
      >> main session_initial
    );

    S.listen s;
end
