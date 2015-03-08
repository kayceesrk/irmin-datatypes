(* Testing LWW Register *)

open Printf
open Lwt
open Irmin_unix
open Irmin_datatypes

let (>>=) = Lwt.bind


module Path = Irmin.Path.String_list
module Lww = Lww_register.Make(struct type t = Path.t end)(Tc.Int)(Path)
let key = Lww.create [ "local"; "key1" ]

let main () =
  let store = Irmin.basic (module Irmin_git.FS) (module Lww.Contents) in
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true() in

  Irmin.create store config task

  >>= fun b1 -> printf "Branch 1: Set value to 10\n"; Lww.update b1 key 10
  >>= fun () -> Lww.read_exn b1 key
  >>= fun v -> printf "Branch 1: Read value: %d\n" v; return ()

  >>= fun () -> Irmin.clone_force task (b1 "cloning the store") "test"

  >>= fun b2 -> Lww.read_exn b2 key
  >>= fun v -> printf "Branch 2: Read value: %d\n" v; return ()
  >>= fun () -> printf "Branch 2: Set value to 20\n"; Lww.update b2 key 20

  >>= fun () -> Irmin.merge_exn "Merge b2 into b1" b2 ~into:b1

  >>= fun () -> Lww.read_exn b1 key
  >>= fun v -> printf "Branch 1: Read value: %d\n" v; return ()

let () = Lwt_unix.run (main ())
