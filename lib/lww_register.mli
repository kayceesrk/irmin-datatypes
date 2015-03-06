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

  include Irmin.Contents.S
  (** The type of LWW registers. *)

  type value
  (** The type of value stored in the register *)

  val read : t -> value option Lwt.t
  (** Read the register. Return [None] if the register does not exist *)

  val read_exn : t -> value Lwt.t
  (** Read the register. Raise [TODO: Which?] exception if the register does not exist *)

  val update : t -> value -> unit Lwt.t
  (** Update the register value *)

  val stats : unit -> stats
  (** Obtain global statistics on register operations *)
end

module type Config = sig
  val conf : Irmin.config
  val task : string -> Irmin.task
end

module Make
   (RW: Irmin.RW_MAKER)
   (K: Irmin.Hum.S)
   (V: Irmin.Hash.S)
   (P: Irmin.Path.S)
   (C: Config)
 : S with type value = V.t
      and type t = K.t
      and module Path = P
