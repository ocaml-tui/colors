type ansi = [ `ansi of int ]
type rgb = [ `rgb of int * int * int ]
type lrgb = [ `lrgb of float * float * float ]
type xyz = [ `xyz of float * float * float ]
type luv = [ `luv of float * float * float ]
type uv = [ `uv of float * float ]
type color = [ ansi | rgb | xyz | luv | uv ]

val pp : Format.formatter -> color -> unit

module ANSI : sig
  val to_rgb : ansi -> int * int * int
end

module White_reference : sig
  val d65 : xyz
end

module Linear_RGB : sig
  val linearize : rgb -> lrgb
  val delinearize : lrgb -> rgb
  val to_xyz : lrgb -> xyz
end

module XYZ : sig
  val to_linear_rgb : xyz -> lrgb
  val to_uv : xyz -> uv
  val to_luv_with_ref : xyz -> wref:xyz -> luv
  val to_luv : xyz -> luv
end

module LUV : sig
  val to_xyz_with_ref : luv -> wref:xyz -> xyz
  val to_xyz : luv -> xyz
  val blend : luv -> luv -> mix:float -> luv
end

module RGB : sig
  val blend : rgb -> rgb -> mix:float -> rgb
end
