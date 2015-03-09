(* Testing LWW Register *)

open Printf
open Lwt
open Irmin_unix
open Irmin_datatypes

let (>>=) = Lwt.bind

module Git = Irmin_git.AO(Git.Memory)
module Config = struct
  let conf = Irmin_git.config ()
  let task = task
end
module Path = Irmin.Path.String_list
module Lww = Lww_register.Make(Tc.Int)(Path)

let key = ["local"; "Register"]

let main () =
  let store = Irmin.basic (module Irmin_git.Memory) (module Lww) in

  Irmin.create store Config.conf Config.task >>= fun b1 ->

  Lww.create 0 >>= fun r0 ->
  Irmin.update (b1 "Set key to r0 (val = 0)") key r0 >>= fun () ->

  printf "Clone branch 1 into branch 2\n";
  Irmin.clone_force Config.task (b1 "cloning the store") "test" >>= fun b2 ->

  Irmin.read_exn (b2 "Fetch register") key >>= fun r1 ->
  Lww.update r1 1 >>= fun r2 ->
  Irmin.update (b2 "Set key to r2 (val = 1)") key r2 >>= fun () ->

  Irmin.read_exn (b1 "Fetch register") key >>= fun r3 ->
  Lww.read_reg r3 >>= fun v ->
  printf "Reg value before merge = %d\n" v;

  printf "Merge branch 2 into branch 1\n";
  Irmin.merge_exn "Merge b2 into b1" b2 ~into:b1 >>= fun () ->

  Irmin.read_exn (b1 "Fetch register") key >>= fun r4 ->
  Lww.read_reg r4 >>= fun v ->
  printf "Reg value after merge = %d\n" v; return ()

let () = Lwt_unix.run (main ())
