open Lwt
open Session
open Horse_manager
open Horse_greeter

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  module Session = Session.Tcp(S)

  let logger_ip = Ipaddr.V4.of_string_exn "127.0.0.1"

  module Logger = struct
    let log console udpv4  message =
      C.log_s console message >>
      S.UDPV4.write udpv4 ~dest_ip:logger_ip ~dest_port:5514 (Cstruct.of_string message)
  end

  let horseos = Horse_manager.create

  let start c s =

    let log = Logger.log c (S.udpv4 s) in

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
          let on_close reason =
                Horse_manager.remove_user horseos username;
                log (Printf.sprintf "%s has quit (%s)\n" username reason) in
            let session = Session.on_close session_initial on_close in
              Horse_manager.add_user horseos username;
              log (Printf.sprintf "%s joined" username) >>
              write_userinfo session
          >> join [ listen_input session username; relay_messages session ]
        )
      ) in

    S.listen_tcpv4 s ~port:4444 (fun flow ->

      let dst, dst_port = S.TCPV4.get_dest flow in
      let message = Printf.sprintf ("Got a connection from %s on port %d") (Ipaddr.V4.to_string dst) dst_port in
      log message >>

      let session_initial = Session.of_flow flow in
      Session.write session_initial Horse_greeter.ascii
      >> main session_initial
    );

    S.listen s;
end
