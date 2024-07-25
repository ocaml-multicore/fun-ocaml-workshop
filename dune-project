(lang dune 3.15)

(name ray_tracer)

(generate_opam_files true)

(source
 (github ocaml-multicore/fun-ocaml-workshop))

(authors "Vesa Karvonen" "Carine Morel" "Arthur Wendling")

(maintainers "Vesa Karvonen" "Carine Morel" "Arthur Wendling")

(license LICENSE)

(package
 (name ray_tracer)
 (synopsis "Ray tracer in OCaml")
 (description
  "Inspired by the Ray Tracing In One Weekend tutorial (https://raytracing.github.io/books/RayTracingInOneWeekend.html).")
 (depends 
 (ocaml (>= 4.14))
 (graphics (>= 5.1.2))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
