(*
 * Copyright (c) 2014 Benjamin Farinier <benjamin.farinier@ens-lyon.fr>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** A naive implementation of mergeable sets. *)

type stats = {
  ops: int;
  reads     : int;
  writes    : int;
}
(** Statistic values. *)

val string_of_stats: stats -> string
(** Pretty-print the stats. *)

module type S = sig

  type elt
  (** The elements of the sets. *)

  include Irmin.Contents.S
  (** The type of sets. *)

  val create : unit -> t Lwt.t
  (** Create a new set. *)

  val cardinal : t -> int Lwt.t
  (** Return the size of the set [t]. *)

  val is_empty : t -> bool Lwt.t
  (** Return true if the given set [t] is empty, false
      otherwise. *)

  val add : t -> elt -> t Lwt.t
  (** Returns a set with [elt] added. *)

  val remove : t -> elt -> t Lwt.t
  (** Returns a set with [elt] removed. *)

  val mem : t -> elt -> bool Lwt.t
  (** Returns true if [elt] is present in the set. *)

  val dump : t -> elt list Lwt.t
  (** Dump the contents of the set. *)

  val stats : unit -> stats
  (** Print global statistics on set operations. *)

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
    (C: Config)
  : S with type elt = V.t
       and module Path = P
