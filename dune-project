(lang dune 3.7)

(name tanenbaum)
(version 0.1.0)

(generate_opam_files true)

(source
 (github sixstring982/tanenbaum))

(authors "Sixstring982")

(maintainers "Sixstring982")

(license BSD-3-Clause)

(documentation https://sixstring982.github.io/tanenbaum)

(package
 (name tanenbaum)
 (synopsis "OCaml Advent of Code starter project")
 (depends
  ocaml
  dune
  (angstrom (>= 0.15.0))
  (cmdliner (>= 1.2.0))
  (core (>= 0.15.0))
  (piaf (>= 0.1.0))
  (fmt (>= 0.9.0))
  (yojson (>= 2.1.0))
 )
 (tags ("advent-of-code")))
