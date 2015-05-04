open Lwt
open Session

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  module Session = Session.Tcp(S)

  module Horse_greeter = struct
    let horse_ascii = "welcome to HorseOS 0.02          |\\    /|\n                              ___| \\,,/_/\n                           ---__/ \\/    \\\n                          __--/     (D)  \\\n                          _ -/    (_      \\\n                         // /       \\_ / ==\\\n   __-------_____--___--/           / \\_ O o)\n  /                                 /   \\==/`\n /                                 /\n||          )                   \\_/\\\n||         /              _      /  |\n| |      /--______      ___\\    /\\  :\n| /   __-  - _/   ------    |  |   \\ \\\n |   -  -   /                | |     \\ )\n |  |   -  |                 | )     | |\n  | |    | |                 | |    | |\n  | |    < |                 | |   |_/\n  < |    /__\\                <  \\\n  /__\\                       /___\\\n\nplease enter a username: "

    let greet session = Session.write session horse_ascii
  end

  module Horse_manager = struct

    type t = { messages : bytes Lwt_condition.t; users : (bytes, unit) Hashtbl.t }

    let create = { messages = Lwt_condition.create () ; users = Hashtbl.create 10 }

    let broadcast_message os username message =
      Lwt_condition.broadcast (os.messages) (Printf.sprintf "%s: %s\n" username message);
      Lwt.return_unit

    let wait_for_messages os = Lwt_condition.wait os.messages

    let get_userinfo os = Hashtbl.fold ( fun user _ acc -> Printf.sprintf "%s * %s \n" acc user ) os.users "Horses in the stable:\n"

    let user_exists os username = Hashtbl.mem os.users username

    let remove_user os username =
      Lwt_condition.broadcast os.messages "%s has quit...\n";
      Hashtbl.remove os.users username

    let add_user os username =
      Hashtbl.add os.users username ();
      Lwt_condition.broadcast os.messages (Printf.sprintf "%s joined\n" username)

  end

  let horseos = Horse_manager.create

  let start c s =

    let log message = C.log c message in

    let rec listen_input session username =
      Session.read session (Horse_manager.broadcast_message horseos username)
        >> listen_input session username in

    let rec relay_messages session =
      Horse_manager.wait_for_messages horseos
        >>= fun message -> Session.write session message
        >> relay_messages session in

    let write_userinfo session = Session.write session (Horse_manager.get_userinfo horseos) in

    let main session_initial =
      Session.read session_initial ( fun username ->
        if Horse_manager.user_exists horseos username then
          Session.write session_initial (Printf.sprintf "There's already a user called %s please try again.\n" username) >>
          Session.close session_initial "bad username"
        else
        (
          let on_close reason = log (Printf.sprintf "%s has quit (%s)\n" username reason);
                Horse_manager.remove_user horseos username in
            let session = Session.on_close session_initial on_close in
              log (Printf.sprintf "%s joined" username);
              Horse_manager.add_user horseos username;
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
