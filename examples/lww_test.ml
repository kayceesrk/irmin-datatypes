(* Testing LWW Register *)

open Printf
open Irmin_datatypes

let (>>=) = Lwt.bind

module Git = Irmin_git.RW(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = Irmin_unix.task
end

module HashInt = struct
  include Tc.Int
  let to_raw = Tc.write_cstruct (module Tc.Int)
  let of_raw = Tc.read_cstruct (module Tc.Int)
  let of_hum = int_of_string
  let to_hum = string_of_int
  let has_kind _ = false
  let digest = of_raw
end

module Path = Irmin.Path.String_list
module Lww = Lww_register.Make(Git)(HashInt)(HashInt)(Path)(Config)

let main =
  Lww.update 0 10
  >>= fun _ -> Lww.read_exn 0
  >>= fun i -> print_int i; Lwt.return ()

let () = Lwt_main.run main
