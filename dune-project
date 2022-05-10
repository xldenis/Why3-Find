(lang dune 3.1)
(generate_opam_files true)
(using dune_site 0.1)

(name why3find)

(authors "Loïc Correnson")
(maintainers "CEA-LIST/LSL")
(license LGPL)
(homepage "https://git.frama-c.com/dev/why3find")
(source (uri "https://git.frama-c.com/dev/why3find.git"))

(package
  (name why3find)
  (version 1.0)
  (synopsis "A Why3 Package Manager")
  (description
    "The why3find utility is designed for managing \
     packages for why3 developpers and associated \
     OCaml extracted code.")
  (depends
     (dune (>= 3.0))
     (ocaml (>= 4.12.0)))
  (sites
    (lib packages)
    (share resources))
  (tags (topics "why3")))
