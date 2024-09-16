(lang dune 3.15)

(name ray_tracer)

(generate_opam_files true)

(source
 (github ocaml-multicore/fun-ocaml-workshop))

(authors "Vesa Karvonen" "Carine Morel" "Arthur Wendling")

(maintainers "Vesa Karvonen" "Carine Morel" "Arthur Wendling")

(license MIT)

(package
 (name ray_tracer)
 (synopsis "Ray tracer in OCaml")
 (description
  "Inspired by the Ray Tracing In One Weekend tutorial (https://raytracing.github.io/books/RayTracingInOneWeekend.html).")
 (depends
  (ocaml
   (>= 4.14))
  (graphics
   (>= 5.1.2))
  (ppx_deriving_yojson
   (>= 3.7.0))
  (saturn_lockfree
   (>= 0.5.0))))

(package
 (name funocaml_protocol)
 (synopsis "FunOCaml Multicore Workshop: actor/server protocol")
 (depends
  (yojson
   (>= 2.0.0))
  (ppx_deriving_yojson
   (>= 3.7.0))
   (ray_tracer
   (= :version))))

(package
 (name funocaml_actor_shared)
 (synopsis "FunOCaml Multicore Workshop: actor library (shared)")
 (depends
  (funocaml_protocol
   (= :version))
  (cohttp
   (>= 6.0.0~beta2))
  (imagelib
   (>= 20221222))
  (base64
   (>= 3.5.1))
  (uri
   (>= 4.4.0))))

(package
 (name funocaml_actor_eio)
 (synopsis "FunOCaml Multicore Workshop: actor library for Eio")
 (depends
  (funocaml_actor_shared
   (= :version))
  (cohttp-eio
   (>= 6.0.0~beta2))
   (saturn
    (and (>= 0.5.0) :with-test))
   (eio_main
    (and (>= 1.0.0) :with-test))))

(package
 (name funocaml_actor_picos_io_cohttp)
 (synopsis
  "FunOCaml Multicore Workshop: actor library for Picos through IO Cohttp")
 (depends
  (funocaml_actor_shared
   (= :version))
  (picos_io_cohttp
   (>= 0.5.0))
   (picos_mux
    (and (>= 0.5.0) :with-test))))

(package
 (name funocaml_server)
 (synopsis "FunOCaml Multicore Workshop: server")
 (depends
  (funocaml_protocol
   (= :version))
  (dream
   (>= 1.0.0~alpha6))
  (js_of_ocaml
   (>= 5.8.2))
   (brr
    (>= 0.0.6))
    (color 
    (>= 0.2.0))
    (ppx_blob
    (>= 0.8.0))
    (tyxml
    (>= 4.6.0))))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
