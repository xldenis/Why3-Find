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
     (dune-site (>= 3.4.1))
     (why3 (>= 1.7.0))
     (ocaml (>= 4.12.0))
     (yojson (>= 1.7.0))
     (zmq (>= 5.0.0))
     (terminal_size (>= 0.2.0)))
  (sites
    (lib packages)
    (share resources))
  (tags (topics "why3")))
