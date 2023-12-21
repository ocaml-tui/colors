(**
   The majority of the following code to convert colors between RGB, Linear
   RGB, XYZ, and LUV, was ported over from the go-colorful library.

   Ref: https://github.com/lucasb-eyer/go-colorful/blob/6e6f2cdd7e293224a813cb9d5411a81ca5eb3029/colors.go
*)

type ansi = [ `ansi of int ]
type rgb = [ `rgb of int * int * int ]
type lrgb = [ `lrgb of float * float * float ]
type xyz = [ `xyz of float * float * float ]
type luv = [ `luv of float * float * float ]
type uv = [ `uv of float * float ]
type color = [ ansi | rgb | xyz | luv | uv ]

let pp fmt t =
  match t with
  | `ansi i -> Format.fprintf fmt "ANSI(%d)" i
  | `rgb (r, g, b) -> Format.fprintf fmt "RGB(%d,%d,%d)" r g b
  | `lrgb (r, g, b) -> Format.fprintf fmt "LinearRGB(%.4f,%.4f,%.4f)" r g b
  | `xyz (x, y, z) -> Format.fprintf fmt "XYZ(%.4f,%.4f,%.4f)" x y z
  | `luv (l, u, v) -> Format.fprintf fmt "LUV(%.4f,%.4f,%.4f)" l u v
  | `uv (u, v) -> Format.fprintf fmt "UV(%.4f,%.4f)" u v

module ANSI = struct
  let to_rgb (`ansi i) =
    let i = Int.(min (max 0 i) (Array.length Ansi_table.to_rgb - 1)) in
    Ansi_table.to_rgb.(i)
end

module White_reference = struct
  (* standard reference white point *)
  let d65 = `xyz (0.95047, 1.00000, 1.08883)
end

module Linear_RGB = struct
  let linearize v =
    if v < 0.04045 then v /. 12.92 else Float.pow ((v +. 0.055) /. 1.055) 2.4

  let linearize (`rgb (r, g, b)) =
    `lrgb
      ( r |> Float.of_int |> linearize,
        g |> Float.of_int |> linearize,
        b |> Float.of_int |> linearize )

  let delinearize v =
    if v <= 0.0031308 then 12.92 *. v
    else (1.055 *. Float.pow v (1.0 /. 2.4)) -. 0.055

  let delinearize (`lrgb (r, g, b)) =
    `rgb
      ( r |> delinearize |> Float.to_int,
        g |> delinearize |> Float.to_int,
        b |> delinearize |> Float.to_int )

  let to_xyz (`lrgb (r, g, b)) =
    let x =
      (0.41239079926595948 *. r) +. (0.35758433938387796 *. g)
      +. (0.18048078840183429 *. b)
    in
    let y =
      (0.21263900587151036 *. r) +. (0.71516867876775593 *. g)
      +. (0.072192315360733715 *. b)
    in
    let z =
      (0.019330818715591851 *. r)
      +. (0.11919477979462599 *. g) +. (0.95053215224966058 *. b)
    in
    `xyz (x, y, z)
end

module XYZ = struct
  let to_linear_rgb (`xyz (x, y, z)) =
    let r =
      (3.2409699419045214 *. x) -. (1.5373831775700935 *. y)
      -. (0.49861076029300328 *. z)
    in
    let g =
      (-0.96924363628087983 *. x)
      +. (1.8759675015077207 *. y)
      +. (0.041555057407175613 *. z)
    in
    let b =
      (0.055630079696993609 *. x)
      -. (0.20397695888897657 *. y) +. (1.0569715142428786 *. z)
    in
    `lrgb (r, g, b)

  let to_uv (`xyz (x, y, z)) =
    let denom = x +. (15.0 *. y) +. (3.0 *. z) in
    if denom = 0.0 then `uv (0.0, 0.0)
    else
      let u = 4.0 *. x /. denom in
      let v = 9.0 *. y /. denom in
      `uv (u, v)

  let to_luv_with_ref (`xyz (_, y, _) as xyz) ~wref:(`xyz (_, wref1, _) as wref)
      =
    let l =
      if y /. wref1 <= 6.0 /. 29.0 *. 6.0 /. 29.0 *. 6.0 /. 29.0 then
        y /. wref1 *. (29.0 /. 3.0 *. 29.0 /. 3.0 *. 29.0 /. 3.0) /. 100.0
      else (1.16 *. Float.cbrt (y /. wref1)) -. 0.16
    in
    let (`uv (ubis, vbis)) = to_uv xyz in
    let (`uv (un, vn)) = to_uv wref in
    let u = 13.0 *. l *. (ubis -. un) in
    let v = 13.0 *. l *. (vbis -. vn) in
    `luv (l, u, v)

  (* use d65 white as reference point by default.
     http://www.fredmiranda.com/forum/topic/1035332
     http://en.wikipedia.org/wiki/Standard_illuminant *)
  let to_luv xyz = to_luv_with_ref xyz ~wref:White_reference.d65
end

module LUV = struct
  let to_xyz_with_ref (`luv (l, u, v)) ~wref:(`xyz (_, wref1, _) as wref) =
    let y =
      if l <= 0.08 then
        wref1 *. l *. 100.0 *. 3.0 /. 29.0 *. 3.0 /. 29.0 *. 3.0 /. 29.0
      else wref1 *. Float.pow ((l +. 0.16) /. 1.16) 3.
    in
    let (`uv (un, vn)) = XYZ.to_uv wref in
    if l != 0.0 then
      let ubis = (u /. (13.0 *. l)) +. un in
      let vbis = (v /. (13.0 *. l)) +. vn in
      let x = y *. 9.0 *. ubis /. (4.0 *. vbis) in
      let z = y *. (12.0 -. (3.0 *. ubis) -. (20.0 *. vbis)) /. (4.0 *. vbis) in
      `xyz (x, y, z)
    else `xyz (0.0, 0.0, 0.0)

  let to_xyz luv = to_xyz_with_ref luv ~wref:White_reference.d65

  let blend (`luv (l1, u1, v1)) (`luv (l2, u2, v2)) ~mix =
    let mix = Float.(min (max 0. mix) 1.) in

    let l = l1 +. (mix *. (l2 -. l1)) in
    let u = u1 +. (mix *. (u2 -. u1)) in
    let v = v1 +. (mix *. (v2 -. v1)) in

    `luv (l, u, v)
end

module RGB = struct
  let blend c1 c2 ~mix =
    let mix = Float.(min (max 0. mix) 1.) in

    let luv1 = c1 |> Linear_RGB.linearize |> Linear_RGB.to_xyz |> XYZ.to_luv in
    let luv2 = c2 |> Linear_RGB.linearize |> Linear_RGB.to_xyz |> XYZ.to_luv in

    LUV.blend luv1 luv2 ~mix |> LUV.to_xyz |> XYZ.to_linear_rgb
    |> Linear_RGB.delinearize
end
