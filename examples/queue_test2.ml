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
module Queue = Merge_queue.Make(Irmin_mem.AO)(Irmin.Hash.SHA1)(Tc.Int)(Path)(Config)
module Store =
  Irmin.Make(Irmin_mem.AO)(Irmin_mem.RW)(Queue)(Irmin.Ref.String)(Irmin.Hash.SHA1)

let key = ["local"; "queue"]

let main () =
  let config = Irmin_git.config ~bare:true () in
  Store.Repo.create config >>= Store.master task >>= fun b1 ->

  Queue.create () >>= fun q1 ->
  Queue.push q1 0 >>= fun q2 ->
  Store.update (b1 "Set key to q2 (len = 1)") key q2 >>= fun () ->

  printf "Clone branch 1 into branch 2\n";
  Store.clone_force Config.task (b1 "cloning the store") "test" >>= fun b2 ->

  Store.read_exn (b2 "Fetch queue") key >>= fun q3 ->
  Queue.push q3 1 >>= fun q4 ->
  Store.update (b2 "Set key to q4 (len = 2)") key q4 >>= fun () ->

  printf "Merge branch 2 into branch 1\n";
  Store.merge_exn "Merge b2 into b1" b2 ~into:b1 >>= fun () ->

  Store.read_exn (b1 "Fetch queue") key >>= fun q5 ->
  Queue.length q5 >>= fun l ->
  printf "Queue length = %d\n" l; return ()

let () = Lwt_unix.run (main ())
