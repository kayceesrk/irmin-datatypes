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
module Set = Merge_set.Make(Git)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)

let key = ["local"; "set"]

let main () =
  let store = Irmin.basic (module Irmin_git.Memory) (module Set) in

  Irmin.create store Config.conf Config.task >>= fun b1 ->

  Set.create () >>= fun s ->
  Set.add s 0 >>= fun s ->
  Set.add s 1 >>= fun s ->
  Irmin.update (b1 "update") key s >>= fun () ->
  Set.dump s >>= fun eltList ->
  printf "Initial set = ";
  List.iter (printf "%d ") eltList;
  printf "\n";

  printf "Clone branch 1 into branch 2\n";
  Irmin.clone_force Config.task (b1 "cloning the store") "test" >>= fun b2 ->

  Set.remove s 0 >>= fun s ->
  Set.add s 2 >>= fun s ->
  Irmin.update (b1 "update") key s >>= fun () ->
  Set.dump s >>= fun eltList ->
  printf "Set on branch 1 = ";
  List.iter (printf "%d ") eltList;
  printf "(removed 0, added 2)\n";

  Irmin.read_exn (b2 "Fetch set") key >>= fun s ->
  Set.remove s 1 >>= fun s ->
  Set.add s 3 >>= fun s ->
  Irmin.update (b2 "update") key s >>= fun () ->
  Set.dump s >>= fun eltList ->
  printf "Set on branch 2 = ";
  List.iter (printf "%d ") eltList;
  printf "(removed 1, added 3)\n";

  printf "Merge branch 2 into branch 1\n";
  Irmin.merge_exn "Merge b2 into b1" b2 ~into:b1 >>= fun () ->

  Irmin.read_exn (b1 "Fetch set") key >>= fun s ->
  Set.dump s >>= fun eltList ->
  printf "Merged list = ";
  List.iter (printf "%d ") eltList;
  printf "\n"; return ()


let () = Lwt_unix.run (main ())
