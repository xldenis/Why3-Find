# Why-3 Library Manager

The `why3find` utility is dedicated to the management of Why-3 packages.

## Why-3 Packages

From `why3find` point of view, a why3 package is a collection of why3 source
files, configuration files and drivers that are installed at predefined site(s).

Packages are meant to have dependencies. Typically package `A` may depend on
why3 source files from some other package `B`. Hence, `why3find` knows how to
generate suitable options for `why3` to be able to find all the necessary sources.
Packages may also have extracted `OCaml` code.

Commands for browsing packages:

    $ why3find where       # location of global repository
    $ why3find shared      # location of why3find shared resources
    $ why3find list        # list all installed packages
    $ why3find query PKG   # details on package PKG

## Package Development

A typical Why-3 package project consists of user-edited files
and generated files from `why3find`.

A new package PKG (usually a lowercase identifier) is typically initiated
by the following command:

    $ why3find init PKG [DIR]

This will create a directory `DIR` (or `PKG` by default) with
some default files and directories. These files are intended to be
further edited by the package developpers:

    DIR/.gitignore     # Can be initiated by why3find init
    DIR/dune-project   # Can be initiated by why3find init
    DIR/PKG/**/*.mlw   # Why-3 source files of package PKG

The package directory will also be populated by generated files that contains
important data for your packages:

    DIR/why3find.json  # Package configuration, generated by why3find config
    DIR/dune           # Dune package installer, generated by why3find install
    DIR/META.json      # Package metadata, generated by why3find install
    DIR/PKG/**/proof.json  # Proof certificates, generated by why3find prove

Those files shall _not_ be edited by the package developers, but shall
be committed in your Git repository or any other version management system.

Finally, many others files will be generated during package development, but
it is _not_ recommended to commit them into your version management system:

    DIR/.why3find      # Prover cache, generated by why3find prove
    DIR/html/**        # Package documentation, generated by why3find doc
    DIR/lib/dune,*.ml  # OCaml extracted code, generated by why3find extract
    DIR/PKG/**/why3session.xml # Why-3 sessions (generated by why3find prove and why3)
    DIR/PKG/**/why3shapes.gz   # Why-3 shapes (generated by why3find prove and why3)

## Package Configuration

Most `why3find` commands support common options, for which default values can be
configured for the entire package. This is achieved by `why3find config` command.
The generated file `why3find.json` is used by `why3find` to locate the root directory
and to store its default configuration options.

By default, `why3find` commands will look for a `why3find.json` file into all
the parents of the current working directory. If not found, the current working directory
will be taken as the package root. Otherwise, you can specify by hand the package root with
option `--root DIR`.

The common options are given below:

    --root DIR            # Specifies the package root directory
    --extra-config CFG    # Extra why3 configuration file
    -p|--package PKG      # Package to depend on
    -P|--prover PRV       # Prover to be used in proofs
    -T|--transf TRANS     # Transformation to be used in proofs
    -D|--driver DRV       # Extraction driver to be used for OCaml

The command `why3find config` can be used to manage the package configuration.
Typicall examples are:

    why3find config -l               # Show current config
    why3find config […] -s           # Add packages, provers, etc. and save
    why3find config --remove […] -s  # Remove packages, provers, etc. and save

## Package Proving

The `why3find prove` command is used to prove why3 lemmas and other proof
obligations.  Each why3 proof obligation is associated to a _proof certificate_
that is generated by `why3find prove` and store in a `proof.json` file. The
command will try to discharge each proof obligation with one of the configured
provers. Alternatively, it will try to apply one of the registered
transformations and recursively prove all the generated sub-goals.

*Proof Certificates* fulfill two different purposes: they can be used as a hint
database for updating proofs, or they can be used to report _how_ some proof
obligation has been finally proved. Hence, proof certificates are very much like
Why-3 sessions, but with much less information details: they contains only
prover timeout information, transformations with no-arguments, and at most one
proving strategy for each goal. Management of proof certificates is controlled
by the following options:

    why3find prove -f    # Force proof reconstruction from scratch (no hints)
    why3find prove -u    # Update proofs by using current certificate as hints
    why3find prove -r    # Replay proofs (no update, no proof completion)
    why3find prove -m    # Complete failed proofs or minimize transformation hints

*Interactive Proving* is activated by `why3find prove -i` and will launch the
standard Why-3 IDE in case of unproved goals. Notice that Why-3 sessions will be
automatically generated in this case. However, proof certificates will _not_ be
updated after exiting Why-3 IDE: this interactive mode is meant for debugging
purpose, and you will have to re-launch `why3find prove` to update the proof
certificates when your Why-3 specifications have been fixed.

*Prover Cache* is locally used to speed up prover attempts, unless option `-c`
is provided. Number of parallel provers can be specified by option
`-j N`.  Alternatively, you can also use `why3find config -j N -s`, in which
case your _personal_ Why-3 configuration `~/.why3.conf` is updated accordingly.

*Prover Calibration* is a strategy used by `why3find` in order to reduce the
instability in proof sessions introduced when using machines with different
performances. Such differences are frequently observed between different
packages or among several developers in the same package. Inside a given proof
certificate, proof times and timeouts are always given _relatively_ to some
reference computer named the _master_ machine.

*The Master Machine* is configured by using `why3find config -m -s` ; you can
also use `why3find config -v` to evaluate the velocity of your local computer
with respect to the master machine.

