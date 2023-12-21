(lang dune 3.11)
(using mdx 0.4)

(name colors)

(generate_opam_files true)

(source
 (github leostera/colors))

(authors "Leandro Ostera <leandro@abstractmachines.dev>")

(maintainers "Leandro Ostera <leandro@abstractmachines.dev>")

(license MIT)

(package
 (name colors)
 (synopsis "A pure OCaml library for manipulating colors across color spaces")
 (depends (ocaml (>= "4.08.0")))
 (tags
  (color "color spaces" "color transformations" "color blending" luv xyz rgb "linear rgb" srgb uv "reference colors")))
