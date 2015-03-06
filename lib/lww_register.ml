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

module Log = Log.Make(struct let section = "QUEUE" end)

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

module type S = sig
  include Irmin.Contents.S
  type value
  val read     : t -> value option Lwt.t
  val read_exn : t -> value Lwt.t
  val update   : t -> value -> unit Lwt.t
  val stats    : unit -> stats
end

module type Config = sig
  val conf : Irmin.config
  val task: string -> Irmin.task
end

module Make
  (RW: Irmin.RW_MAKER)
  (K: Irmin.Hum.S)
  (V: Irmin.Hash.S)
  (P: Irmin.Path.S)
  (Config: Config)
= struct

  module Path = P

  module C = struct
    module M = Tc.Pair (Time)(V)
    include M

    let new_value value = (Time.now(), value)

    let compare (x,u) (y,v) =
      let r = Time.compare x y in
      if r = 0 then V.compare u v else r

    let to_hum (t,v) =
      String.concat ["("; Time.to_string t; ", ";
                     V.to_hum v; ")"]

    let of_hum s =
      let re = Re_pcre.regexp "\\((.*),(.*)\\)" in
      let arr = Re_pcre.extract re s in
      (Time.of_string @@ arr.(0), V.of_hum @@ arr.(1))

    let to_raw v = Tc.write_cstruct (module M) v
    let of_raw cs = Tc.read_cstruct (module M) cs

    (* KC: Is this right? *)
    let digest = of_raw
    (* KC: Is this right? *)
    let has_kind _ = false
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
    module S = RW(K)(C)

    include S

    let create () = create Config.conf Config.task
    let read t k = incr_read (); S.read t k
    let read_exn t k = incr_read (); S.read_exn t k
    let read_free t k = S.read_exn t k
    let update t k v = incr_write (); S.update t k v
  end

  type lww_reg = K.t

  module T = Tc.Biject (K)
    (struct
      type t = lww_reg
      let to_t v = v
      let of_t v = v
    end)
  include T

  type value = V.t

  let update key value =
    Store.create () >>= fun store ->
      Store.update (store "update") key @@ C.new_value value

  let read key =
    Store.create ()
    >>= fun store -> Store.read (store "read") key
    >>= function
        | None -> return None
        | Some (_,value) -> return (Some value)


  let read_exn key =
    Store.create ()
    >>= fun store -> Store.read_exn (store "read_exn") key
    >>= fun (_, value) -> return value

  let merge : Path.t -> t option Irmin.Merge.t =
    let merge ~old r1 r2 =
      if K.compare r1 r2 > 0 then ok r1
      else ok r2
    in fun _path -> Irmin.Merge.option (module T) merge

  let stats () =
    let reads = get_read () in
    let writes = get_write () in
    { ops = reads + writes; reads; writes }

end
