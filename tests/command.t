--------------------------------------------------------------------------
--- All Commands
--------------------------------------------------------------------------
  $ why3find --help
  why3find [-h|--help]
  why3find [-v|--version]
  why3find where
  why3find shared
  why3find init
  why3find list
  why3find query [PKG...]
  why3find config [OPTIONS] PROVERS
  why3find prove [OPTIONS] PATH...
  why3find doc [OPTIONS] PATH...
  why3find extract [OPTIONS] MODULE...
  why3find server OPTIONS
  why3find worker OPTIONS
  why3find install PKG PATH...
  why3find uninstall [PKG...]
  why3find compile [-p PKG] FILE
  why3find ide [-p PKG] FILE
  why3find replay [-p PKG] FILE
  why3find CMD [ARGS...]
--------------------------------------------------------------------------
--- why3find where
--------------------------------------------------------------------------
  $ why3find where --help
  USAGE:
  
    why3find where [-a|--all]
  
  DESCRIPTION:
  
    Prints installation site(s).
  
--------------------------------------------------------------------------
--- why3find shared
--------------------------------------------------------------------------
  $ why3find shared --help
  USAGE:
  
    why3find shared [-a|--all]
  
  DESCRIPTION:
  
    Prints shared resources site(s).
  
--------------------------------------------------------------------------
--- why3find init
--------------------------------------------------------------------------
  $ why3find init --help
  USAGE:
  
    why3find init PKG [DIR]
  
  DESCRIPTION:
  
    Create templates for dune-project and git-ignore for package PKG.
    Files are created in directory DIR (default ./PKG).
  
--------------------------------------------------------------------------
--- why3find list
--------------------------------------------------------------------------
  $ why3find list --help
  USAGE:
  
    why3find list
  
  DESCRIPTION:
  
    Prints all installed packages.
  
  $ why3find query --help
  USAGE:
  
    why3find query [PKG...]
  
  DESCRIPTION:
  
    Query why3 package location.
  
  OPTIONS:
  
    -p print package paths only
    -L prints -L <path> for all dependencies
    -r recursive mode, query also dependencies
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find compile
--------------------------------------------------------------------------
  $ why3find compile --help
  USAGE:
  
    why3find compile [OPTIONS] FILE
  
  DESCRIPTION:
  
    Compile the given file(s) using why3 prove command.
  
  OPTIONS:
  
    -v|--verbose print why3 command
    -p|--package PKG package dependency
    --extra-config FILE additional configuration file
  
--------------------------------------------------------------------------
--- why3find ide
--------------------------------------------------------------------------
  $ why3find ide --help
  USAGE:
  
    why3find ide [OPTIONS] FILE
  
  DESCRIPTION:
  
    Run why3 ide on the given file.
  
    Also loads the « hammer » strategy.
  
  OPTIONS:
  
    -v|--verbose print why3 command
    -p|--package PKG package dependency
    --extra-config FILE additional configuration file
  
--------------------------------------------------------------------------
--- why3find replay
--------------------------------------------------------------------------
  $ why3find replay --help
  USAGE:
  
    why3find replay [OPTIONS] MODULE...
  
  DESCRIPTION:
  
    Executes why3 replay with the specified arguments.
  
  OPTIONS:
  
    -v|--verbose print why3 command
    -p|--package PKG package dependency
    --extra-config FILE additional configuration file
  
  $ why3find extract --help
  USAGE:
  
    why3find extract [OPTIONS] MODULE...
  
  DESCRIPTION:
  
    Extract OCaml and generate Dune file.
  
  OPTIONS:
  
    --root DIR change to directory
    --extra-config CFG extra why3 config
    --package PKG add package dependency
    --driver DRV add extraction driver
    -p  same as --package
    -D  same as --driver
    --lib Generate PKG.lib library instead of PKG
    -l PKG Additional OCaml library dependency
    -o destination directory (default "lib")
    -v print why3 extract command
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find config
--------------------------------------------------------------------------
  $ why3find config --help
  USAGE:
  
    why3find config [OPTIONS]
  
  DESCRIPTION:
  
    Configuration of the local package.
    By default, report on the current configuration.
  
  OPTIONS:
  
    --root DIR change to directory
    --extra-config CFG extra why3 config
    --package PKG add package dependency
    --time TIME median proof time
    --depth DEPTH proof search limit
    --prover PRV add automated prover
    --transf TRANS add transformation
    --driver DRV add extraction driver
    --remove remove items from configuration
    -p  same as --package
    -t  same as --time
    -d  same as --depth
    -P  same as --prover
    -T  same as --transf
    -D  same as --driver
    -c force cache update
    -j JOBS max parallel provers
    -m calibrate provers (master)
    -v evaluate prover velocity (local)
    -l list final configuration
    -s save project configuration
    --relax relax prover version constraints
    --strict save strict prover versions
    --detect update why3 config detect
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find prove
--------------------------------------------------------------------------
  $ why3find prove --help
  USAGE:
  
    why3find prove [OPTIONS] PATH...
  
  DESCRIPTION:
  
    Prove all why3 files and directories accessible from PATH.
  
  OPTIONS:
  
    --root DIR change to directory
    --extra-config CFG extra why3 config
    --package PKG add package dependency
    --time TIME median proof time
    --depth DEPTH proof search limit
    --prover PRV add automated prover
    --transf TRANS add transformation
    --driver DRV add extraction driver
    --remove remove items from configuration
    -p  same as --package
    -t  same as --time
    -d  same as --depth
    -P  same as --prover
    -T  same as --transf
    -D  same as --driver
    -c force cache update
    -j JOBS max parallel provers
    -f force rebuild proofs
    -u update proofs (default)
    -r replay proofs (no update)
    -m minimize proofs (or update)
    -i run Why3 IDE on error(s) (implies -s)
    -s save why3 session
    -h report hypotheses and axioms
    --host HOST Proof Server host (default: none)
    --port PORT Proof Server port (default: 5555)
    --server URL Proof Server address (default: "tcp://HOST:PORT")
    --polling TIME server polling interval (default 1.0s)
    --trace Trace server protocol
    --modules list results by module
    --theories list results by theory
    --goals list results by goals
    --proofs list proofs by goals
    --stdlib report hypotheses from stdlib
    --extern report assumed external symbols
    --builtin report assumed builtin symbols
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find doc
--------------------------------------------------------------------------
  $ why3find doc --help
  USAGE:
  
    why3find doc [OPTIONS] PATH...
  
  DESCRIPTION:
  
    Generate HTML documentation.
  
    Includes all why3 sources and markdown pages
    accessible from PATH.
  
  
  OPTIONS:
  
    --root DIR change to directory
    --extra-config CFG extra why3 config
    --package PKG add package dependency
    --driver DRV add extraction driver
    -p  same as --package
    -D  same as --driver
    -t document title (default none)
    -u output generated URI
    -o destination directory (default "html")
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find server
--------------------------------------------------------------------------
  $ why3find server --help
  USAGE:
  
    why3find server [OPTIONS]
  
  DESCRIPTION:
  
    Establishes a proof server.
  
  OPTIONS:
  
    --stats Print cache disk usage
    --prune AGE Prune cache generations older than AGE
    --address URL server address (default "tcp://*:5555")
    --database DIR Database (default "why3server")
    --polling TIME server polling interval (default 1.0s)
    --trace Trace server protocol
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find worker
--------------------------------------------------------------------------
  $ why3find worker --help
  USAGE:
  
    why3find worker [OPTIONS]
  
  DESCRIPTION:
  
    Provides a worker for the specified proof server.
  
  OPTIONS:
  
    -c force cache update
    -j JOBS max parallel provers
    --server URL proof server address (default "tcp://localhost:5555")
    --polling TIME server polling interval (default 1.0s)
    --trace Trace server protocol
    --local no calibration (use local times)
    --reftime TIME set calibration time (default 0.5s)
    --sequential use sequential calibration algorithm
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find install
--------------------------------------------------------------------------
  $ why3find install --help
  USAGE:
  
    why3find install [OPTIONS] PKG PATH...
  
  DESCRIPTION:
  
  
    Install the package PKG at the topmost installation site.
  
    Package dependencies and configuration are taken from the
    local project, or from the command line:
  
      PKG/**         all why3 source files
      PKG/**/*.mlw   specified why3 source files
      **/*.cfg       extra why3 configuration
      **/*.drv       OCaml extraction drivers
  
    If no source file is given, all why3 source files
    in directory PKG will be installed.
  
    Unless --no-doc is specified, documentation in './html'
    directory is also installed.
  
  OPTIONS:
  
    --dune Generate dune installer (default)
    --global Install in global repository (why3find where)
    --lib DIR extraction directory (why3find extract -o DIR)
    --doc DIR doc output directory (why3find doc -o DIR)
    --no-doc Do not install documentation
    -help  Display this list of options
    --help  Display this list of options
  
--------------------------------------------------------------------------
--- why3find uninstall
--------------------------------------------------------------------------
  $ why3find uninstall --help
  USAGE:
  
    why3find uninstall [PKG...]
  
  DESCRIPTION:
  
    Remove all specified packages from topmost installation site.
  
--------------------------------------------------------------------------
--- Generic Command
--------------------------------------------------------------------------
  $ why3find CMD --help
  USAGE:
  
    why3find CMD [ARGS...]
  
  DESCRIPTION:
  
    Execute command "CMD" with wrapped arguments.
  
  OPTIONS:
  
    --p|--package PKG : pass --library=<path> for the package
    --configs : pass also --extra-config-file=<CFG> options
    --drivers : pass also --driver=<DRV> options
  
--------------------------------------------------------------------------
