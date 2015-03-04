open Mirage

let main = foreign "Unikernel.Main" (console @-> stackv4 @-> job)

let xen =
  try match Sys.getenv "XEN" with
    | "" -> false
    | _ -> true
  with Not_found -> false

let stackv4 =
  if xen then
    direct_stackv4_with_dhcp default_console tap0
  else
    socket_stackv4 default_console [Ipaddr.V4.any]

let () =
  register "horseos" [
      main $ default_console $ stackv4
  ]
