(lang dune 2.0)

(generate_opam_files)

(source
 (github emillon/switch-builder))

(maintainers "Etienne Millon <me@emillon.org>")

(package
 (name switch-builder)
 (synopsis "Tools to build opam switches and dune workspaces")
 (depends
  (ocaml
   (>= 4.13))
  ocamlgraph
  opam-repository))
