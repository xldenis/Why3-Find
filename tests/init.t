  $ why3find init mypkg
  Generated $TESTCASE_ROOT/mypkg/.gitignore
  Generated $TESTCASE_ROOT/mypkg/dune-project

  $ find mypkg | sort
  mypkg
  mypkg/.gitignore
  mypkg/dune-project

  $ cat mypkg/.gitignore
  why3session.xml*
  why3shapes.gz*
  /.why3find
  /_build
  /html
  /lib

  $ cat mypkg/dune-project
  (lang dune 3.1)
  (generate_opam_files true)
  (using dune_site 0.1)
  (name mypkg)
  (authors "Author Name(s)")
  (maintainers "Maintainer Name(s)")
  (license LICENSE)
  (package
   (name mypkg)
   (synopsis "short synopsis")
   (description "longer description")
   (depends why3find))

  $ why3find init mypkg
  #### Suggested .gitignore:
  why3session.xml*
  why3shapes.gz*
  /.why3find
  /_build
  /html
  /lib
  ####
  #### Suggested dune-project:
  (lang dune 3.1)
  (generate_opam_files true)
  (using dune_site 0.1)
  (name mypkg)
  (authors "Author Name(s)")
  (maintainers "Maintainer Name(s)")
  (license LICENSE)
  (package
   (name mypkg)
   (synopsis "short synopsis")
   (description "longer description")
   (depends why3find))
  ####
