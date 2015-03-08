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

type stats = {
  ops: int;
  reads     : int;
  writes    : int;
}
(** Statistic values. *)

val string_of_stats: stats -> string
(** Pretty-print the stats. *)

module type S = sig

  type t
  (** The type of LWW registers. *)

  type key
  (* The type of key in the target store *)

  type value
  (** The type of value stored in the register *)

  module Contents : Irmin.Contents.S
  (** The type of contents internally stored in the register. *)

  type 'a store = string -> ('a, key, Contents.t) Irmin.t
  (** Type alias for convenience. *)

  val create : key -> t
  (** Create a new register with the given key. *)

  val read : [<`RO|`HRW|`BC] store -> t -> value option Lwt.t
  (** Read the register. Return [None] if the register does not exist *)

  val read_exn : [<`RO|`HRW|`BC] store -> t -> value Lwt.t
  (** Read the register. Raise [TODO: Which?] exception if the register does not exist. *)

  val update : [<`HRW|`BC] store -> t -> value -> unit Lwt.t
  (** Update the value of register. *)

  val stats : unit -> stats
  (** Obtain global statistics on register operations *)
end

module type Config = sig
  val conf : Irmin.config
  val task : string -> Irmin.task
end

module Make
   (K: sig type t end)
   (V: Tc.S0)
   (P: Irmin.Path.S)
 : S with type value = V.t
      and type key = K.t
      and module Contents.Path = P
