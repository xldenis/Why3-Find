# Why-3 Library Manager

The `why3find` utility is dedicated to the management of Why-3 packages.  In
short, from `why3find` point of view, a why3 package is a collection of why3
source files and associated documentation and OCaml extracted code that are installed at
predefined site(s). It is designed to be fully compatible with Dune and OPAM.

- [Why-3 Packages](#why-3-packages)
- [Package Development](#package-development)
- [Package Configuration](#package-configuration)
- [Package Proving](#package-proving)
- [Package Documentation](#package-documentation)
- [OCaml Code Extraction](#ocaml-code-extraction)

## Why-3 Packages

Packages are meant to have dependencies. Typically package `A` may depend on
why3 source files from some other package `B`. Hence, `why3find` knows how to
generate suitable options for `why3` to be able to find all the necessary sources.
Packages may also have extracted `OCaml` code.

Commands for browsing packages:

    $ why3find where       # location of global repository
    $ why3find shared      # location of why3find shared resources
    $ why3find list        # list all installed packages
    $ why3find query PKG   # details on package PKG

Packages are installed or uninstalled thanks to the following commands:

    $ why3find install PKG
    $ why3find uninstall PKG...

By default, installation generates a `dune` file at the root directory
of the package with all the necessary installation instructions.
However, it is also possible to install files directly to the `why3find` repository,
by using `why3find install --global PKG`, without using any `dune` intermediate file.

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
Typical examples are:

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

*Proving Strategy* for building proof certificates is a heuristic based on
user-defined median time, registered provers and transformations. It consists
in several _rounds_ tried in sequence until proof completion:

1. Fast sequential provers: each configured prover is tried in _sequence_ with a
   short timeout (1/5 of the median time).

2. Parallel provers: all provers are tried in _parallel_ with the median time as
   timeout.

3. Transformation: each configured or selected transformation is tried in
   _sequence_ ; the first one that is applicable terminates the strategy for
   this goal and the generated sub-goals are scheduled for proof completion.

4. Final parallel long try: if no transformation applies, all provers are
   finally tried in _parallel_ with a larger timeout (2 times the median time).

In case the final round fails to complete the proof, the goal is marked stuck
and all its parent goals are marked incomplete. *Remark:* incomplete proof
certificates are also stored in order to be used as hints for further proof
lookup. However, a transformation with all its sub-goals marked « stuck » would
be removed.

The median time is one second by default and can be modified with `-t TIME` or
configured using `why3find config -t TIME -s`. The median time is specified in
(fraction of) seconds relatively to the *master* machine, Cf. prover calibration
below.

Transformation search is pruned after a maximal depth of 10 levels, which can
be modified with `-d DEPTH` or configured using `why3find config -d DEPTH -s`.

*Prover Cache* is stored in hidden file `.why3find` at the root of the package
directory.  You can bypass access to the cache with option `--no-cache`,
however, it will still be updated for further use.

*Parallel Proving* is used to run different provers parallel on the different
cores of your machine. You can specify the number of parallel provers with
option `-j N`.  Alternatively, you can also use `why3find config -j N -s` to
configure it locally, in which case your _personal_ Why-3 configuration
`~/.why3.conf` is updated accordingly. Parallel proving is also performed among
the different goals and proof obligations to be proved. However, priority is put
on goals that were incomplete with respect to the previously known proof
certificates. This strategy makes detection of stuck goals faster when debugging
proofs. During execution in a terminal, `why3find prove` displays a progression
status with the following format:

    $ why3find prove […]
    P/Q/S/R goal goal …

`P` is the number of complete proofs to be replayed; `Q` is the number of stuck
or partial proofs to be complete; finally `S` and `R` are respectively the
number of schedules and currently running prover tasks. The topmost active goals
for proof completion are also given for user feedback.

*Prover Calibration* is a strategy used by `why3find` in order to reduce the
instability in proof sessions introduced when using machines with different
performances. Such differences are frequently observed between different
packages or among several developers in the same package. For a given proof
certificate file, proof times and timeouts are always given _relatively_ to some
reference computer named the _master_ machine. The velocity of a prover on a
local machine with respect to the *master* machine is evaluated by running a
reference problem for some parameter `n` on both machines. This velocity is then
used to convert local times into reference times _wrt_ to the master machine or
_vice et versa_. The `why3find` commands related to prover calibration are :

    $ why3find config -m -s     # Calibrate provers on the master machine
    $ why3find config -v        # Evaluate velocity of provers on a local machine

It is highly recommended to update all proofs on the *master* machine with
`why3find prove -f` after modifying the proof calibration. Usually, you calibrate
provers once at the very beginning of the project.

## Package Documentation

The `why3find doc` command is used to build HTML documentation for the package.
It can be used for the entire package or only a selection of source files or
directories. Unless specified, generated files go to the `html` sub-directory
of the root package directory:

    $ why3find doc PATH...

Each `PATH` can be either a single Why3 `*.mlw` file, a single markdown `*.md`
file or a directory to be recursively processed. Markdown files are treated
just like a plain `(**…*)` comment inside Why3 source files.

Generated documentation output is a flat directory organized as follows:

    html/fonts                   # Icon fonts
    html/style.css               # CSS resources
    html/script.js               # JavaScript resources
    html/page.html               # Documentation for markdown file
    html/pkg.file.index.html     # Documentation for Why3 source file
    html/pkg.file.proof.html     # Proof documentation for source file
    html/pkg.file.Module.html    # Documentation of module or theory

The generated files for Icons, CSS and JavaScript are imported from the
`why3find share` directory.  They can be replaced by your own files for further
customization.

*Documentation Structure* follows the order of documentation blocs, which shall
be put inside `(** … *)` comments. Module and Theory documentation are placed
into separated files. The documentation bloc immediately before the module or
theory declaration (without empty line between) is put as an introduction in the
module or theory documentation file. For instance:

    (** This goes into the index page *)

    (** This is the header of module M's page *)
    module M
      …
    end

    (** This goes back to the index page *)

*Basic Markdown Format* can be used in plain markdown page and to why3
documentation comments. The following documentation snippets show examples:

    # Header (level 1)
    ## Header (level 2, etc.)

    Text with *emphasis* or **bold**.
    Can also use _emphasis_ or __bold__.
    Source `code`.

    Itemized lists:
      - item A
      - item B

    References:
      - {qid} unambiguous reference (including modules)
      - {t:qid} types
      - {l:qid} functions and predicates
      - {p:qid} properties (lemmas or axiom)
      - {v:qid} program values

Notice tha why-3 references works across packages, assuming documentation of
external packages have been properly installed.

*Code Sections* can be delimited with fold/unfold. This is especially useful to hide
some information details, typically proof details. Examples:

    lemma Thm: forall x,y. …
      (*proof*) by x = y -> … (*qed*)

Section delimiters can have various format:

    (*proof*) … (*qed*)   # folded by default
    (*[Title]*)           # open a section with « Title » (folded)
    (*[Title]-*)          # initially folded section
    (*[Title]+*)          # initially unfolded section
    (*/[End]*)            # close the section with « End » closing title

Actually, `(*proof*)…(*qed*)` is a shortcut for `(*[Proof]-*)…(*/[Qed]*)`.
Nested sections are allowed. Notice also that sections are authorized to overlap
code's nested parentheses, although it is not recommended to do so for readability
reason. For instance `(*proof*) begin (*qed*) end` is valid,
but makes the generated documentation difficult to read.

*Cloned Modules and Theories* sections are automatically inserted for clones, with
a copy of the cloned symbols signature, and links to their original definitions.

*Proof Results* based on proof certificates generated by `why3find prove`
are also inserted after each goal identifier. Proof summaries are also inserted
for modules, theories and clones.

*Consolidated Hypotheses* for each module is also generated and added to the
proof documentation. It consists of all the parameters (abstract types or logic
symbols), abstract values and hypotheses (axioms) the module actually depends
on. Axiom summaries are also inserted for modules, theories and clones.

*Extracted and Built-in Symbols* from prover drivers and extraction drivers are
also taken into account when consolidating hypotheses. Extracted symbols are
reported but not counted as parameters. Erased axioms or builtin symbols from
prover drivers are considered to be built-in symbols and are not counted as
actual hypotheses or paremeters.

## OCaml Code Extraction

The `why3find extract` command can be used to extract OCaml code from the
current Why3 package. More precisely, the command calls `why3 extract` with
options corresponding to the project configuration and from the `why3find extract`
command line. Moreover, the `why3find extract` command also generates a `dune` file
for compiling and installing the library of extracted OCaml code.

You shall specify the the list of Why-3 modules (`MODULE`) to be extracted on the command line,
as follows:

    why3find extract MODULE...

All modules shall belong to the same package. Options can be passed to
`why3find extract` command:

    why3find extract -d PKG   # additional OCaml package dependency
    why3find extract -o DIR   # extraction output directory (default is "lib")
    why3find extract -s       # generate symbols for ppx_why3find

Symbol extraction can be generated to be used with `ppx_why3find` OCaml package.
From an external OCaml client code, you can easily invoke the generated types and
values from your Why-3 package using the `ppx_why3find` module.

In OCaml client code, just add `ppx_why3find` to the list of PPX rewriters to
the `dune` configuration of client code:

    (libray MyClientCode
       (preprocess (pps why3find.ppx …))
        …)

Inside the client OCaml code, you can use the following extension points:

    [%%why3use "<why3-module>" ]               (** open scope *)
    [%%why3use "<why3-module> as <uident>" ]   (** open scope *)

    [%why3! (typ,...) <why3-type> ]        (** type identifier *)
    [%why3? <why3-constructor>(pat,...) ]  (** constructor pattern *)
    [%why3  <why3-constructor>(exp,...) ]  (** construtor expression *)
    [%why3  exp.<why3-field> ]             (** field access *)
    [%why3  <why3-value> ]                 (** extracted symbol *)

Scope rules follows the Why-3 usage. Notice that full why3 module names must be
written between quotes since they are _not_ part of the OCaml syntax. In other
extension points, Why-3 (qualified) identifiers follows the OCaml syntax and
don't need quotes.
