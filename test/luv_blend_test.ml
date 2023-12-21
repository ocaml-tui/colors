open Colors

let () =
  let blend = RGB.blend (`rgb (0, 0, 0)) (`rgb (255, 255, 255)) ~mix:0.5 in
  match blend with
  | `rgb (107, 107, 107) -> Printf.printf "rgb_blend_test: OK"
  | other ->
      Format.eprintf "rgb_blend_test: unexpected color %a" Colors.pp
        (other :> Colors.color)
