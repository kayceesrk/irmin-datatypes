module type DIFF = sig
  include Tc.S0
  (** The type of base. *)

  type diff
  (** The type of diff. *)

  val patch : diff -> t -> t Lwt.t
  (** Apply a patch. Patch must always be accepted. *)

  val diff : t -> t -> diff Lwt.t
  (** Obtain a diff between two versions. *)
end

module Make
  (D: DIFF)
  (P: Irmin.Path.S)
 : Irmin.Contents.S
   with type t = D.t
    and module Path = P
