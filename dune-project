(lang dune 3.6)
(generate_opam_files true)
(name opam-monorepo)

(cram enable)

(source (github tarides/opam-monorepo))
(license ISC)
(authors "Anil Madhavapeddy" "Nathan Rebours" "Lucas Pluvinage" "Jules Aguillon")
(maintainers "anil@recoil.org")
(documentation "https://tarides.github.io/opam-monorepo")

(package
 (name opam-monorepo)
 (synopsis "Assemble and manage fully vendored Dune repositories")
 (description "The opam monorepo plugin provides a convenient interface to bridge the
opam package manager with having a local copy of all the source
code required to build a project using the dune build tool.")
 (depends
  (ocaml (>= 4.13.0)))
 (conflicts
  (dune-build-info (or (= 2.7.0) (= 2.7.1)))
  (dune-configurator (or (= 2.7.0) (= 2.7.1)))))
