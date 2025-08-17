(lang dune 3.15)
(name octal)

(generate_opam_files)
(homepage "https://github.mit.edu/arch-sec/secsep-octal")

(package
 (name octal)
 (synopsis "SecSep's inference, checker, and transformation on Octal")
 (authors "Shixin Song" "Tingzhen Dong")
 (license MIT)
 (depends ocaml dune core z3 sexp))
