open Lwt
open Irmin.Merge.OP

module type ABELIAN = sig
  include Tc.S0
  (** The type of abelian group. *)

  val zero : t
  (** Zero element of the group. *)

  val (+:) : t -> t -> t
  (** Addition operation for the group. *)

  val (-:) : t -> t -> t
  (** Difference operation for the group. *)
end

module Make (A: ABELIAN) (P: Irmin.Path.S) = struct
  include A

  module Path = P

  let merge: Path.t -> t option Irmin.Merge.t =
    let merge ~old v1 v2 =
      old () >>= function
      | `Conflict _ | `Ok None -> conflict "merge"
      | `Ok (Some old) -> ok @@ old +: (v1 -: old) +: (v2 -: old)
    in fun _path -> Irmin.Merge.option (module A) merge
end
