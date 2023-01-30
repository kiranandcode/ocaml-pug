(lang dune 3.6)
(cram enable)

(generate_opam_files)

(name pug)

(package (name pug)
  (synopsis "Pug-lite parser & compiler for OCaml")
  (authors "gopiandcode")
  (maintainers "gopiandcode")
  (homepage "https://github.com/gopiandcode/pug")
  (bug_reports "https://github.com/gopiandcode/pug/issues")
  (license AGPL-3.0-or-later)
  (depends
    angstrom
    tyxml
    cmdliner))
