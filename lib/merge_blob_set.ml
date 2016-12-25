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

let list_dedup ?(compare=Pervasives.compare) t =
  let t = List.sort compare t in
  let rec aux acc = function
    | []      -> List.rev acc
    | [x]     -> aux (x :: acc) []
    | x::(y::_ as tl) ->
      match compare x y with
      | 0 -> aux acc tl
      | _ -> aux (x :: acc) tl
  in
  aux [] t

exception Empty

type stats = {
  ops: int;
  reads: int;
  writes: int;
}

let string_of_stats t =
  Printf.sprintf "%i\t%f\t%f%!"
    t.ops
    ((float t.reads)  /. (float t.ops))
    ((float t.writes) /. (float t.ops))

module type S = sig
  include Irmin.Contents.S
  type elt
  val create : unit -> t Lwt.t
  val cardinal : t -> int Lwt.t
  val is_empty : t -> bool Lwt.t
  val add : t -> elt -> t Lwt.t
  val remove : t -> elt -> t Lwt.t
  val mem : t -> elt -> bool Lwt.t
  val dump : t -> elt list Lwt.t
  val stats : unit -> stats
end

module type Config = sig
  val conf: Irmin.config
  val task: string -> Irmin.task
end

module Make
    (AO: Irmin.AO_MAKER)
    (K: Irmin.Hash.S)
    (V: Tc.S0)
    (P: Irmin.Path.S)
    (Config: Config)
= struct

  module C = struct

    module Tc_set = Tc.Set(V)
    include Tc_set

    type diff = {adds: Tc_set.t; removes: Tc_set.t}

    module S = Set.Make(V)

    let diff s1 s2 =
      return {adds = S.diff s1 s2;
              removes = S.diff s2 s1}

    let patch d s =
      return @@ S.diff (S.union s d.adds) d.removes
  end

  let (incr_read, incr_write, get_read, get_write) =
    let count_read = ref 0 in
    let count_write = ref 0 in
    (
      (fun () -> incr count_read),
      (fun () -> incr count_write),
      (fun () -> !count_read),
      (fun () -> !count_write)
    )

  module Store = struct

    module S = AO(K)(C)

    include S

    let create () =
      create Config.conf

    let read t k =
      incr_read ();
      S.read t k

    let read_exn t k =
      incr_read ();
      S.read_exn t k

    let read_free t k =
      S.read_exn t k

    let add t v =
      incr_write ();
      S.add t v

  end

  type set = { card : int; root : K.t }
  module SetV = Set.Make(V)

  module S = struct

    module T = Tc.Biject (Tc.Pair(Tc.Int)(K))
      (struct
         type t = set
         let to_t (card, root) = {card; root}
         let of_t {card; root} = (card, root)
      end)
    include T

    type diff = C.diff

    let diff {root = r1; _} {root = r2; _} =
      Store.create () >>= fun store ->
      Store.read_exn store  r1 >>= fun s1 ->
      Store.read_exn store  r2 >>= fun s2 ->
      C.diff s1 s2

    let patch d {root; _} =
      Store.create () >>= fun store ->
      Store.read_exn store  root >>= fun s ->
      C.patch d s >>= fun res_set ->
      Store.add store res_set >>= fun new_root ->
      return {root = new_root; card = SetV.cardinal res_set}
  end

  include Builder.Make(S)(P)

  type elt = V.t

  let is_empty { card; _ } = return (card == 0)
  let cardinal { card; _} = return card

  let dump { card; root } =
    if card == 0 then return []
    else Store.create () >>= fun store ->
         Store.read_exn store  root >>= fun set ->
         return @@ SetV.elements set

  let create () =
    Store.create () >>= fun store ->
    Store.add store  (SetV.empty) >>= fun root ->
    return {card = 0; root}

  let add {root; _} elt =
    Store.create () >>= fun store ->
    Store.read_exn store  root >>= fun set_val ->
    let new_set = SetV.add elt set_val in
    let card = SetV.cardinal new_set in
    Store.add store  new_set >>= fun new_root ->
    return @@ {root = new_root; card}

  let remove {root; _} elt =
    Store.create () >>= fun store ->
    Store.read_exn store  root >>= fun set_val ->
    let new_set = SetV.remove elt set_val in
    let card = SetV.cardinal new_set in
    Store.add store  new_set >>= fun new_root ->
    return @@ {root = new_root; card}

  let mem {root; _} elt =
    Store.create () >>= fun store ->
    Store.read_exn store  root >>= fun set_val ->
    return @@ SetV.mem elt set_val

  let stats () =
    let reads = get_read () in
    let writes = get_write () in
    { ops = reads + writes; reads; writes }
end
