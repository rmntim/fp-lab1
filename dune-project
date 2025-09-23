(lang dune 3.20)

(name labwork1)

(generate_opam_files true)

(source
 (github rmntim/fp-lab1))

(authors "Roman Timoshkin <rmntim@yandex.ru>")

(maintainers "Roman Timoshkin <rmntim@yandex.ru>")

(license MIT)

(package
 (name labwork1)
 (synopsis "Functional Programming Labwork 1")
 (description "Functional Programming Labwork 1")
 (depends
  ocaml
  zarith
  (alcotest :with-test)
  (zanuda :with-dev-setup)))
