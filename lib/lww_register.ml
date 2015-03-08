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

(* A last-write-wins register *)

open Lwt
open Core
open Core.Std
open Irmin.Merge.OP

module Log = Log.Make(struct let section = "LWW_REGISTER" end)

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

module type TIME = module type of Time

module Time = struct
  include Tc.Bin_prot0 (struct
    include Time
    let to_json v = Ezjsonm.float @@ to_float v
    let of_json v = of_float @@ Ezjsonm.get_float v
    let bin_size_t v = Bin_prot.Size.bin_size_float @@ to_float v
    let bin_write_t a ~pos c = Bin_prot.Write.bin_write_float a ~pos (to_float c)
    let bin_read_t a ~pos_ref = Bin_prot.Read.bin_read_float a ~pos_ref |> of_float
  end)
  include (Time : TIME with type t := t)
end

module type Config = sig
  val conf : Irmin.config
  val task: string -> Irmin.task
end

module type S = sig
  type t
  type key
  type value
  module Contents : Irmin.Contents.S
  type 'a store = string -> ('a, key, Contents.t) Irmin.t
  val create : key -> t
  val read : [<`RO|`HRW|`BC] store -> t -> value option Lwt.t
  val read_exn : [<`RO|`HRW|`BC] store -> t -> value Lwt.t
  val update : [<`HRW|`BC] store -> t -> value -> unit Lwt.t
  val stats : unit -> stats
end

module Make
  (K: sig type t end)
  (V: Tc.S0)
  (P: Irmin.Path.S)
= struct

  module Contents = struct
    module M = Tc.Pair (Time)(V)
    include M

    module Path = P

    let new_value value = (Time.now(), value)

    let compare (x,u) (y,v) =
      let r = Time.compare x y in
      if r = 0 then V.compare u v else r

    let to_raw v = Tc.write_cstruct (module M) v
    let of_raw cs = Tc.read_cstruct (module M) cs

    let merge : Path.t -> t option Irmin.Merge.t =
      let merge ~old ((t1,_) as v1) ((t2,_) as v2) =
        if Time.compare t1 t2 > 0 then ok v1 else ok v2
      in fun _path -> Irmin.Merge.option (module M) merge
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

  type t = K.t
  type key = K.t
  type value = V.t
  type 'a store = string -> ('a, key, Contents.t) Irmin.t

  let read store reg =
    Irmin.read (store "read") reg
    >>= function
        | None -> return None
        | Some (_,value) -> return (Some value)

  let read_exn store reg =
    Irmin.read_exn (store "read_exn") reg
    >>= fun (_, value) -> return value

  let update store reg value =
    Irmin.update (store "update") reg @@ Contents.new_value value

  let stats () =
    let reads = get_read () in
    let writes = get_write () in
    { ops = reads + writes; reads; writes }

  let create k = k
end
