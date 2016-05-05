(*
 * Copyright (c) 2015 KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

(* Testing LWW Register *)

open Printf
open Lwt
open Irmin_unix
open Irmin_datatypes

let (>>=) = Lwt.bind

module Config = struct
  let conf = Irmin_git.config ()
  let task = task
end
module Path = Irmin.Path.String_list
module Set = Merge_blob_set.Make(Irmin_mem.AO)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)
module Store =
  Irmin.Make(Irmin_mem.AO)(Irmin_mem.RW)(Set)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let key = ["local"; "set"]

let main () =
  let config = Irmin_git.config ~bare:true () in
  Store.Repo.create config >>= Store.master task >>= fun b1 ->

  Set.create () >>= fun s ->
  Set.add s 0 >>= fun s ->
  Set.add s 1 >>= fun s ->
  Store.update (b1 "update") key s >>= fun () ->
  Set.dump s >>= fun eltList ->
  printf "Initial set = ";
  List.iter (printf "%d ") eltList;
  printf "\n";

  printf "Clone branch 1 into branch 2\n";
  Store.clone_force Config.task (b1 "cloning the store") "test" >>= fun b2 ->

  Set.remove s 0 >>= fun s ->
  Set.add s 2 >>= fun s ->
  Store.update (b1 "update") key s >>= fun () ->
  Set.dump s >>= fun eltList ->
  printf "Set on branch 1 = ";
  List.iter (printf "%d ") eltList;
  printf "(removed 0, added 2)\n";

  Store.read_exn (b2 "Fetch set") key >>= fun s ->
  Set.remove s 1 >>= fun s ->
  Set.add s 3 >>= fun s ->
  Store.update (b2 "update") key s >>= fun () ->
  Set.dump s >>= fun eltList ->
  printf "Set on branch 2 = ";
  List.iter (printf "%d ") eltList;
  printf "(removed 1, added 3)\n";

  printf "Merge branch 2 into branch 1\n";
  Store.merge_exn "Merge b2 into b1" b2 ~into:b1 >>= fun () ->

  Store.read_exn (b1 "Fetch set") key >>= fun s ->
  Set.dump s >>= fun eltList ->
  printf "Merged list = ";
  List.iter (printf "%d ") eltList;
  printf "\n"; return ()


let () = Lwt_unix.run (main ())
