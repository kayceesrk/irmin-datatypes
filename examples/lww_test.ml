(* Testing LWW Register *)

open Printf
open Irmin_datatypes

let (>>=) = Lwt.bind

module Git = Irmin_git.AO(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end

module Path = Irmin.Path.String_list
module Lww = Lww_register.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)

let main =
  Lww.create 10
  >>= fun k1 -> Lww.read_exn k1
  >>= fun v -> printf "read value: %d\n" v;
               Lwt.return ()
let () = Lwt_main.run main
