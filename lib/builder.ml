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

open Lwt
open Irmin.Merge.OP

module type DIFF = sig
  include Tc.S0
  type diff
  val patch : diff -> t -> t Lwt.t
  val diff : t -> t -> diff Lwt.t
end

module Make (D: DIFF) (P: Irmin.Path.S) = struct
  include D
  module Path = P

  let merge: Path.t -> t option Irmin.Merge.t =
    let merge ~old v1 v2 =
      old () >>= function
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) ->
          diff v1 old >>= fun d1 ->
          diff v2 old >>= fun d2 ->
          patch d1 old >>= fun res1 ->
          patch d2 res1 >>= fun res ->
          ok res
    in fun _path -> Irmin.Merge.option (module D) merge
end
