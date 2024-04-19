  $ why3find init mypkg
  Generated $TESTCASE_ROOT/mypkg/.gitignore
  Generated $TESTCASE_ROOT/mypkg/dune-project
  Generated $TESTCASE_ROOT/mypkg/mypkg
  Next steps:
    - go into package directory mypkg
    - use why3find config [OPTIONS] to configure
    - populate with mypkg/**/*.mlw source files
    - why3find prove [files...] from anywhere
    - why3find doc|install from root package directory
  
  Package mypkg initialized in directory mypkg

  $ find mypkg | sort
  mypkg
  mypkg/.gitignore
  mypkg/dune-project
  mypkg/mypkg

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
  Generated $TESTCASE_ROOT/mypkg/mypkg
  Next steps:
    - go into package directory mypkg
    - use why3find config [OPTIONS] to configure
    - populate with mypkg/**/*.mlw source files
    - why3find prove [files...] from anywhere
    - why3find doc|install from root package directory
  
  Package mypkg initialized in directory mypkg
