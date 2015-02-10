open Lwt

module Main (C: V1_LWT.CONSOLE) (S: V1_LWT.STACKV4) = struct

  let start c s = 
    C.log c "HorseOS is starting.";
    C.log c "HorseOS is exiting.";
    Lwt.return ()
end
