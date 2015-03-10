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

module Make
  (A: ABELIAN)
  (P: Irmin.Path.S)
 : Irmin.Contents.S
