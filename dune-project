(lang dune 3.0)

(name why3find)

(generate_opam_files true)
(using dune_site 0.1)

; (source (github username/reponame))

(authors "Loïc Correnson")

(maintainers "CEA LIST")

(license LGPL)

(documentation "https://git.frama-c.com/dev/why3find")

(package
  (name why3find)
  (version 0.1)
  (synopsis "A Why3 Package Manager")
  (description
    "The why3find utility is design to manage \
     packages and librairies for why3 developpers.")
  (homepage "https://git.frama-c.com/dev/why3find")
  (depends ocaml dune)
  (sites
    (lib packages)
    (share resources))
  (tags (topics "why3")))
