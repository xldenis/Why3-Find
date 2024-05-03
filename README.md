![](https://git.frama-c.com/dev/why3find/badges/master/pipeline.svg?key_text=Tests)
![](https://git.frama-c.com/dev/why3find/badges/master/coverage.svg?key_text=Coverage)

# Why3find

Why3find provides a collection of utilities to ease the development
of Why3 projects and the distribution of Why3 libraries and
packages. It can be used in place of the traditional `why3` command for most
development tasks, eg. configuring provers, proving files or interactively
debugging proofs. In additional, it can be used to build enhanced documentation,
package and install libraries or setup a distributed solver cloud.

- [Quick Setup](#quick-usage)
- [Why3 Packages](#why3-packages)
- [Package Development](#package-development)
- [Package Configuration](#package-configuration)
- [Package Proving](#package-proving)
- [Package Documentation](#package-documentation)
- [Package Soundness](#package-soundness)
- [OCaml Code Extraction](#ocaml-code-extraction)

## Quick Setup

Let say you have some Why3 source files. Go into their directory and simply
use the following commands:

    $ why3find config --detect
    $ why3find prove

This will configure Why3 with the SMT solvers installed on your computer,
and prove all your Why3 files in the directory. If any some proof failed,
you may try `why3find prove -x` to dump a colored proof context
in your terminal, or `why3find prove -i` to open Why3 IDE to further
debug your proofs. Re-run `why3find prove` until proofs are complete.

At any time, you can generate a complete HTML documentation enhanced with
proof witnesses and axiom dependencies with the following command:

    $ why3find doc

To go further, continue reading this manual and use the following commands:

    $ why3find [-h|--help]            # Help on commands
    $ why3find COMMAND [-h|--help]    # Help on specific command

## Packages

Why3find packages provides a way to share Why3 modules and theories across different
projects. Hence, a package is essentially a collection of WhyML files,
together with configuration files and metadata, including package
dependencies, prover configuration, proof strategies, additional configuration,
etc.

Typically, `why3find` uses package metadata to properly configure Why3 for using
the configured packages when proving and generating documentation. To ease the
management of Why3 packages, `why3find` provides the following commands:

    $ why3find where       # location of global repository
    $ why3find shared      # location of why3find shared resources
    $ why3find list        # list all installed packages
    $ why3find query PKG   # details on package PKG

Packages are installed or uninstalled thanks to the following commands:

    $ why3find install PKG
    $ why3find uninstall PKG...

By default, installation actually only generates a `dune` file at the root
directory of the package with all the necessary installation instructions.
However, it is also possible to install files directly to the `why3find`
repository, by using `why3find install --global PKG`, without using any `dune`
intermediate file.

## Package Development

A typical Why3 package project consists of user-edited files
and generated files from `why3find`.

A new package PKG (usually a lowercase identifier) is typically initiated
by the following command:

    $ why3find init PKG [DIR]

This will create a directory `DIR` (or `PKG` by default) with a default
dune project file and git-ignore configuration. The directory `DIR/PKG` is also
created. The package directory shall be structured as follows:

    DIR
    ├── .why3find             # Why3find local cache     (*not* versioned)
    ├── why3find.json         # Why3find configuration      (versioned)
    ├── dune-project          # Dune project configuration  (versioned)
    ├── lib/**                # OCaml extracted files       (versioned)
    └── PKG                   # Package source directory
        ├── **/*.mlw          # Why3 source files           (versioned)
        ├── **/*/proof.json   # Why3find proof witness      (versioned)
        ├── **/*/why3session.xml  # Why3 internal files   (*not* versioned)
        └── **/*/why3shapes.gz    # Why3 internal files   (*not* versioned)

You shall put your Why3 source files in this sub-directory in order
to be compliant with Why3 global naming policy. Consider for instance
a package `foo` with a source file `foo/bar.mlw` declaring a module `Jazz`,
this module will be named `foo.bar.Jazz` in every place, either from
the package `foo` or from another package depending on `foo`.

Strictly speaking, if your package is _not_ intended to be used as a package
dependency of another Why3 project, you might omit the `PKG` sub-directory and
put your source files directly at the root directory. However, the `why3find
install` command will not work as expected and you will have to manage
installation by your own means. Other `why3find` commands will work as expected,
typically for proving, extracted OCaml code and generating documentation.

## Package Configuration

Most `why3find` commands support common options, for which default values can be
configured for the entire package. This is achieved by `why3find config`
command. The generated file `why3find.json` is used by `why3find` to locate the
root directory and to store its default configuration options.

By default, `why3find` commands will look for a `why3find.json` file into all
the parents of the current working directory. If not found, the current working
directory will be taken as the package root. Otherwise, you can specify by hand
the package root with option `--root DIR`.

The common options are given below:

    --root DIR            # Specifies the package root directory
    --extra-config CFG    # Extra why3 configuration file
    -p|--package PKG      # Package to depend on
    -P|--prover PRV       # Provers to be used
    -T|--tactic TAC       # Tactics to be used
    -D|--driver DRV       # Extraction driver to be used for OCaml

Package, prover, tactic and driver options can be used to specify multiple items
at a time. Finer management is possible using modifier flags. Flag `+` adds
items, flag `-` remove items, flag `=` replace items, flag `n:` move or insert
items starting at position `n`. A flag applies to all elements in the list,
until a new flag is emitted. Moreover, option `none` can be used to remove all
items from the configuration. For instance:

    --prover none         # Remove all provers
    --prover a,b,c        # Use provers a, b and c only
    --prover +a,b         # Add provers a and b to package config
    --prover -a,b,+c      # Remove provers a and b from package config, then add c
    --prover 2:a,b        # Move or insert provers a and b at position 2 and 3
    --prover +a,-b,1:c    # Add prover a, remove b, move or insert c at position 1

Provers can be pinned to precise versions, using `prover@version`. Prover names
are case-insensitive and may also refer to prover shortcuts from Why3
configuration.

The command `why3find config` can be used to manage the package configuration.
Typical examples are:

    why3find config               # Show current config
    why3find config […]           # Update packages, provers, etc.
    why3find config --reset  […]  # Configure from scratch (ignore current config)
    why3find config --default […] # Use all "default" provers available from why3 configuration
    why3find config --detect […]  # Run `why3 config detect` before using all "default" provers
    why3find config --check       # Check consistency between proof certificates and configuration

When using `--default` or `--detect` why3find will select the highest available
versions of provers `alt-ergo`, `z3`, `cvc4` and `cvc5` from why3 configuration.
`--detect` will first update why3 configuration by running `why3 config detect`.
Why3find is known to work well with those provers.

## Package Proving

The `why3find prove` command is used to prove why3 lemmas and other proof
obligations. Each why3 proof obligation is associated to a _proof certificate_
that is generated by `why3find prove` and store in a `proof.json` file. The
command will try to discharge each proof obligation with one of the configured
provers. Alternatively, tactics can be used to simplify a goal and recursively
prove all the generated sub-goals. Tactics are Why3 transformations with no
arguments. They shall transform tasks into equivalent sub-tasks, since once a
tactic has been applied, why3find prove will stick on it (no back-tracking).

*Proof Certificates* fulfill different purposes: they are used for proofs replay,
as a hint database for proofs update, and can also be used to report _how_ some proof
obligation has been finally proved. Hence, proof certificates are very much like
Why3 sessions, but with much less information details: they contain only
prover results with time information, tactics, and contain at most one
proving strategy for each subgoal. Management of proof certificates is controlled
by the following options:

    why3find prove -f    # Force proof reconstruction from scratch (no hints)
    why3find prove -u    # Update proofs by using current certificate as hints (the default)
    why3find prove -r    # Replay proofs (no update, no proof completion)
    why3find prove -m    # Complete failed proofs and minimize proof trees

*Proof Context* can be printed for unproved goals can be printed
on the terminal by `why3find prove -x`. Unproved goals are highlighted in
red, hypotheses and positive conditions in green and negative conditions
in magenta. Hypotheses and context are kept in a range of 5 lines around goals.
Use `--context n` instead of `-x` to output a context of `n` lines instead.

*Interactive Proving* is activated by `why3find prove -i` and will launch the
standard Why3 IDE in case of unproved goals. Notice that Why3 sessions will be
automatically generated in this case. However, proof certificates will _not_ be
updated after exiting Why3 IDE: this interactive mode is meant for debugging
purpose, and you will have to re-launch `why3find prove` to update the proof
certificates when your Why3 specifications have been fixed.

*Proving Strategy* for building proof certificates is a heuristic based on
user-defined reference time, registered provers and tactics. It consists
in several _rounds_ tried in sequence until proof completion:

1. Fast sequential provers: each configured prover is tried in _sequence_ with a
   short timeout (1/5 of the reference time).

2. Parallel provers: all provers are tried in _parallel_ with the reference time as
   timeout.

3. Tactics: each configured or selected tactic is tried in
   _sequence_ ; the first one that is applicable terminates the strategy for
   this goal and the generated sub-goals are scheduled for proof completion
   (there is no backtracking).

4. Final parallel long try: if no tactic applies, all provers are
   finally tried in _parallel_ with a larger timeout (2 times the reference time).

In case the final round fails to complete the proof, the goal is marked stuck
and all its parent goals are marked incomplete. *Remark:* incomplete proof
certificates are also stored in order to be used as hints for further proof
lookup. However, a tactic-node with all its sub-goals marked « stuck » would
be removed.

The reference time is set to one second by default and can be modified with `-t
TIME` or configured using `why3find config -t TIME`. A fraction of seconds or
suffix time units (`h`,`min`,`s`,`ms`) can be given, eg. `0.5`, `200ms`,
`3min`. The reference time is specified relatively to the *master* machine,
Cf. prover calibration below.

Proof search is pruned after a maximal number of nested levels. The default
depth is 6 and it can be modified with `-d DEPTH` or configured using `why3find
config -d DEPTH`.

When using `why3find prove -i` mode, the Why3 IDE is loaded with custom
proof strategies that mimics the why3find proof strategy:
 - strategy `hammer` (keyboard shortcut `H`) repeatedly applies the provers,
   then try the tactics, then finally retries the provers with a longer time.
 - strategy `provers` (keyboard shortcut `P`) only applies the provers;
 - strategy `unfold` (keyboard shortcut `U`) applies the tactics,
   then go into strategy `hammer`.

*Prover Cache* is stored in hidden file `.why3find` at the root of the package
directory.  You can bypass access to the cache with option `--no-cache`,
however, it will still be updated for further use.

*Parallel Proving* is used to run different provers parallel on the different
cores of your machine. You can specify the number of parallel provers with
option `-j N`.  Alternatively, you can also use `why3find config -j N` to
configure it locally, in which case your _personal_ Why3 configuration
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

    $ why3find config -m     # Calibrate provers on the master machine
    $ why3find config -v     # Evaluate velocity of provers on a local machine

It is highly recommended to update all proofs on the *master* machine with
`why3find prove -f` after modifying the proof calibration. Usually, you calibrate
provers once at the very beginning of the project.

Prover calibration is known to work well with provers `at-ergo`, `z3`, `cvc4`
and `cvc5`. It might also work out-of-the-box for other provers, provided they
support quantified anonymous function theories from the SMTLIB. Please report us
any calibration problem with your favorite provers, if any.

*Proof Server* can be used in order to increase parallelism and stability in
collaborative development. First, you need to launch a centralized `why3find
server` that will serve as a centralized proxy and will manage the proof
database on disk. Then, a number of `why3find client` can dynamically connect
to the server to offer their locally available provers for proving tasks. Any
user can then connect to the server to obtain proof results, by using `why3find
prove --server` options. Hence, proof results can be shared among different
users during development. By launching a large number of clients, you can build
large proof clusters. Prover calibration of each contributing client is used by
the server to ensure the consistency of the distributed proof results.

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

Remark: regular comments inside modules are printed as they are, along with the
code.  However, code comments that live outside modules are not printed since
index pages only contains documentation blocs.

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

Notice that Why3 references works across packages, assuming documentation of
external packages have been properly installed.

*Code Sections* can be delimited with fold/unfold. This is especially useful to hide
some information details, typically proof details. Examples:

    lemma Thm: forall x,y. …
      (*proof*) by x = y -> … (*qed*)

Section delimiters can have various format:

    (*proof*) … (*qed*)   # folded by default
    (*[Title]*)           # open a section with « Title » (initially visible)
    (*[Title]-*)          # open an initially folded section (not visible)
    (*[Title]+*)          # open an initially unfolded section (visible)
    (*/[End]*)            # close current section with « End » closing title

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
actual hypotheses or parameters.

## Package Soundness

When proving a module or when generating the documentation of the package,
`why3find` will also collect all the abstract definitions and properties through
all your dependencies. Indeed, the validity of your proofs generalizes to any
ground instance of abstract definitions that satisfies all the assumed
properties. Although, if no such ground instance exists, your proofs are still
valid from a logical point of view, but they have no useful value. In this respect,
a module with no ground instance can be considered to be _unsound_.

The `why3find` documentation generator promotes the development of _sound_
packages by checking that every module with abstract parameters or assumed
properties is actually refined by a clone instance from some other _sound_
module. In the generated documentation, each module is annotated with its
abstract parameters and assumed properties; known instances are also reported
and soundness is checked by transitive closure. Hence, each abstract module is
expected to be cloned inside a sound module in order to witness the consistency
of its hypotheses.

Modules hypotheses are classified as follows:

- Abstract types, constants, pure logic functions and predicates are _sound_
  since Why3 checks for all types to be inhabited (hence, including arrow types).

- Axioms and declarations of abstract values with non-trivial post-conditions
  are all considered to be possibly _unsound_ unless a sound clone instance of
  the module is witnessed.

- Introduction of `any ...` values inside `let` definitions are considered to be
  always _sound_ since Why3 generates proof obligations to demonstrate that such
  a value exists.

- Any _assumed_ assertion (`assume { _ }`) inside a `let` definition is
  considered to be _always_ unsound since it is not possible to refine such an
  assumption by cloning.

- Any abstract values _inside_ a `let` definition is also considered to be
  _always_ unsound for the same reason.

- Hypotheses and abstract parameters from the Why3 standard library are _all_
  considered to be _sound_.

When proving a module with `why3find prove` you can list all the abstract
parameters and assumed properties involved by your proofs with option `-a`.
Other options are available, see `why3find prove -h` for further details.

## OCaml Code Extraction

The `why3find extract` command can be used to extract OCaml code from the
current Why3 package. More precisely, the command calls `why3 extract` with
options corresponding to the project configuration and from the `why3find
extract` command line. Moreover, the `why3find extract` command also generates a
`dune` file for compiling and installing the library of extracted OCaml code.

You shall specify the the list of Why3 modules (`MODULE`) to be extracted on the
command line, as follows:

    why3find extract MODULE...

All modules shall belong to the same package. Options can be passed to
`why3find extract` command:

    why3find extract -d PKG   # additional OCaml package dependency
    why3find extract -o DIR   # extraction output directory (default is "lib")

The extracted modules are not intended to be directly used from OCaml client
code.  As a rule of thumb you shall write some safe OCaml layer upon the
extracted one, as a separated `dune` library.

## References

« [Packaging proofs with Why3find](https://cea.hal.science/hal-04407129v1) »
L. Correnson, CEA-LIST, JFLA 2024

« [Why3 — Where Programs Meet Provers](https://inria.hal.science/hal-00789533) »
J-C. Filliâtre, A. Paskevich, INRIA, ESOP 2021
