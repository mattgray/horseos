open Printf

type t = { messages : bytes Lwt_condition.t; users : (bytes, unit) Hashtbl.t }

let create = { messages = Lwt_condition.create () ; users = Hashtbl.create 10 }

let broadcast_message os username message =
  Lwt_condition.broadcast (os.messages) (sprintf "%s: %s\n" username message);
  Lwt.return_unit

let wait_for_messages os = Lwt_condition.wait os.messages

let get_userinfo os = Hashtbl.fold (fun user _ acc -> sprintf "%s * %s \n" acc user) os.users "Horses in the stable:\n"

let user_exists os username = Hashtbl.mem os.users username

let remove_user os username =
  Lwt_condition.broadcast os.messages (sprintf "%s has quit...\n" username);
  Hashtbl.remove os.users username

let add_user os username =
  Hashtbl.add os.users username ();
  Lwt_condition.broadcast os.messages (sprintf "%s joined\n" username)
