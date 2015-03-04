open Mirage

let main = foreign "Unikernel.Main" (console @-> stackv4 @-> job)

let stackv4 = socket_stackv4 default_console [Ipaddr.V4.any]

let () =
  register "horseos" [
      main $ default_console $ stackv4
  ]
