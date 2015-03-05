(* Last-write-wins register *)

open Lwt
open Irmin_unix
open Core

let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _ ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

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

module Path = Irmin.Path.String_list
include Tc.Pair (Time)(Tc.Int)

let compare (x,_) (y,_) = Pervasives.compare x y

let new_value value = (Time.now (), value)

let merge _path ~old t1 t2 =
  let open Irmin.Merge.OP in
  if compare t1 t2 < 0 then ok t1 else ok t2

let merge path = Irmin.Merge.option (module Tc.Pair(Time)(Tc.Int)) (merge path)
