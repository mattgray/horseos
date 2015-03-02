open Mirage

let main = foreign "Unikernel.Main" (console @-> stackv4 @-> job)

let stack console = socket_stackv4 console [Ipaddr.V4.any]

let () =
  register "horseos" [
      main $ default_console $ stack default_console
  ]
