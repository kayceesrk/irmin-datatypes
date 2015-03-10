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
