(* Last-write-wins register *)

module Lww_core (Value : Tc.S0) = struct

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

  let compare (x,u) (y,v) =
    let r = Pervasives.compare x y in
    if r == 0 then Pervasives.compare u v
    else r

  let new_value value = (Time.now (), value)

  let merge _path ~old t1 t2 =
    let open Irmin.Merge.OP in
    if compare t1 t2 < 0 then ok t2 else ok t1

  let merge path = Irmin.Merge.option (module Tc.Pair(Time)(Tc.Int)) (merge path)

end

module Int = Lww_core (Tc.Int)
module String = Lww_core (Tc.String)
module Int32 = Lww_core (Tc.Int32)
module Int64 = Lww_core (Tc.Int64)
module Unit = Lww_core (Tc.Unit)
module Bool = Lww_core (Tc.Bool)
