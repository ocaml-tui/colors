# Colors

A pure OCaml library for manipulating colors in different color spaces.

It supports creating colors in several formats:

|         | RGB | Linear RGB | XYZ | LUV |
|---------|-----|-------------|-----|----|
|RGB| create | translate | - | - |
|LinearRGB| translate | create | translate | - |
|XYZ| - | translate | create | translate |
|LUV| - | - | translate | create |

And blending:

```ocaml
open Colors
let black = `rgb (0,0,0)
let white = `rgb (255,255,255)
let gray = RGB.blend black white ~mix:0.5
let () = assert (gray = `rgb (107,107,107))
```
