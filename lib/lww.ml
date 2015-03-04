(* Last-write-wins register *)

open Lwt
open Irmin_unix

let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _ ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()
