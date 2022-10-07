# Why-3 Library Manager

The `why3find` utility is dedicated to the management of interoperable Why-3
libraries.

## Usage

The `why3find` command can be used as a convenient wrapper for the usual `why3` command.
Simply type `why3find` or `why3find -h` to list all commands, and `why3find <cmd> -h`
for detailed instructions of command `<cmd>`.

## Why-3 Packages

From `why3find` point of view, a why3 package is a collection of why3 source
files, configuration files and drivers that are installed at predefined site(s).

Packages are meant to have dependencies, ie. package `A` might depend on
why3 source files from package `B`, recursively. Hence, `why3find` knowns how to
generate suitable options for `why3` to be able to find all the necessary sources.

You may also extract OCaml files from your Why-3 files and make them available as
regular `opam` or `ocamlfind` OCaml packages.

## Package Development

The recommended setup for a new Why-3 package `PKG` is as follows:

1. First, run `why3find init PKG [DIR]` ; this will create templates for
   `Makefile`, `.gitignore` and `dune-project` that you can starts with.

2. Place your Why-3 source files in `DIR/PKG/**/*.mlw` directory ; the `PKG/`
   prefix is necessary to avoid `why3` name clashes between different packages.

3. Now, run `why3find config` to configure your why3 project.

A typical Why-3 package project will be structured as follows:

```
+ DIR/dune-project   This is where you setup your deployment options
+ DIR/dune           This file is generated by why3find extract
+ DIR/META.json      This file is generated by why3find install
+ DIR/PKG/**/*.mlw   This is where your Why-3 source files live
+ DIR/lib/dune       This file is generated by make build for OCaml extraction
+ DIR/lib/*.ml       The extracted OCaml source files
```

All those files shall be managed under version control, although generated files
shall be up-to-data by running `make build`.

## Proof Certificates

The most useful command you want to use for conducting your proofs is `make
fix`.  This command tries to prove all your Why-3 source files with the hammer
and, in case of failure, it opens Why-3 IDE.

The why3 session files are generated from the hammer and shall _not_ be managed
under version control, since the session data highly depends on your local
machine. However, those session files can be taken as proof certificates that
can be checked or replayed by `make check` and `make replay`.

## Package Installation

To install your Why-3 package and its associated extracted OCaml package,
proceed as follows:

1. First, run `make build` to update dune files and rebuild everything.
2. Second, run `make install` ; this will install both the Why-3 and OCaml packages.
