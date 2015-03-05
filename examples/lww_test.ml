(* Testing LWW Register *)

open Lwt
open Irmin_unix
open Irmin_datatypes
open Core

let key = [ "local"; "register1" ]

let read t = Irmin.read (t "Reading the register") key

let write t v =
  let reg_val = Lww_register.new_value v in
  Irmin.update (t "Updating the register") key reg_val

let main () =
  let store = Irmin.basic (module Irmin_git.FS) (module Lww_register) in
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true()  in
  Irmin.create store config task >>= fun t ->
    write t 10 >>= fun () ->
    read t >>= function
      | None -> return_unit
      | Some (t,v) ->
          print_string @@ Time.to_string t;
          print_string " ";
          print_string @@ string_of_int v;
          return_unit

let _ = Lwt_main.run @@ main ()
