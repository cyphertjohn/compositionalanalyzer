(lang dune 2.8)
(name compositional_analyzer)
(using menhir 2.1)


(generate_opam_files true)

(license MIT)
(authors "John Cyphert")
(maintainers "github.com/cyphertjohn")

(package
 (name compositional_analyzer)
 (synopsis "A library for generating symbolic bounds")
 (depends
   (mlgmpidl (>= 1.2.13))
   (z3 (>= 4.8.1))
 )
)