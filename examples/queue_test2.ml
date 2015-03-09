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
module Queue = Merge_queue.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)

let key = ["local"; "queue"]

let main () =
  let store = Irmin.basic (module Irmin_git.Memory) (module Queue) in

  Irmin.create store Config.conf Config.task >>= fun b1 ->

  Queue.create () >>= fun q1 ->
  Queue.push q1 0 >>= fun q2 ->
  Irmin.update (b1 "Set key to q2 (len = 1)") key q2 >>= fun () ->

  printf "Clone branch 1 into branch 2\n";
  Irmin.clone_force Config.task (b1 "cloning the store") "test" >>= fun b2 ->

  Irmin.read_exn (b2 "Fetch queue") key >>= fun q3 ->
  Queue.push q3 1 >>= fun q4 ->
  Irmin.update (b2 "Set key to q4 (len = 2)") key q4 >>= fun () ->

  printf "Merge branch 2 into branch 1\n";
  Irmin.merge_exn "Merge b2 into b1" b2 ~into:b1 >>= fun () ->

  Irmin.read_exn (b1 "Fetch queue") key >>= fun q5 ->
  Queue.length q5 >>= fun l ->
  printf "Queue length = %d\n" l; return ()

let () = Lwt_unix.run (main ())
