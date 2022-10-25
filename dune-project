(lang dune 3.1)
(generate_opam_files true)
(using dune_site 0.1)

(name why3find)

(authors "Loïc Correnson")
(maintainers loic.correnson@cea.fr)
(license LGPL-3.0)
(source (uri "git+https://git.frama-c.com:dev/why3find.git"))
(homepage "https://git.frama-c.com/dev/why3find")
(documentation "https://git.frama-c.com/dev/why3find")
(bug_reports "https://git.frama-c.com/dev/why3find/issues")

(package
  (name why3find)
  (version 1.0)
  (synopsis "A Why3 Package Manager")
  (description
    "The why3find utility is designed for managing \
     packages for why3 developpers and associated \
     OCaml extracted code.")
  (depends
     (dune (>= 3.5))
     (why3 (>= 1.5.1))
     (ocaml (>= 4.12.0)))
  (sites
    (lib packages)
    (share resources))
  (tags (topics "why3")))

(package
  (name ppx_why3find)
  (version 1.0)
  (synopsis "Why3 Extractor PPX")
  (description
    "The ppx_why3find package provides extension points \
     for invoking OCaml extracted code from Why3.")
  (depends
     (dune (>= 3.5))
     (why3 (>= 1.5.1))
     (ocaml (>= 4.12.0)))
  (sites
    (lib packages))
  (tags (topics "why3" "ppx")))
