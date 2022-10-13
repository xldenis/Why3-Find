  $ why3find --help
  why3find [-h|--help]
  why3find [-v|--version]
  why3find where
  why3find shared
  why3find init
  why3find list
  why3find query [PKG...]
  why3find compile [-p PKG] FILE
  why3find ide [-p PKG] FILE
  why3find replay [-p PKG] FILE
  why3find extract [-p PKG] MODULE...
  why3find config [OPTIONS] PROVERS
  why3find prove [OPTIONS] PATH...
  why3find doc [OPTIONS] PATH...
  why3find install PKG PATH...
  why3find uninstall [PKG...]
  why3find CMD [ARGS...]

  $ why3find where --help
  USAGE:
  
    why3find where [-a|--all]
  
  DESCRIPTION:
  
    Prints installation site(s).
  

  $ why3find shared --help
  USAGE:
  
    why3find shared [-a|--all]
  
  DESCRIPTION:
  
    Prints shared resources site(s).
  

  $ why3find init --help
  USAGE:
  
    why3find init PKG [DIR]
  
  DESCRIPTION:
  
    Create templates for dune-project and git-ignore for package PKG.
    Files are created in directory DIR (default ./PKG).
  

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
  

  $ why3find compile --help
  USAGE:
  
    why3find compile [OPTIONS] FILE
  
  DESCRIPTION:
  
    Compile the given file(s) using why3 prove command.
  
  OPTIONS:
  
    -v|--verbose print why3 command
    -p|--package PKG package dependency
    --extra-config FILE additional configuration file
  

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
  

  $ why3find replay --help
  USAGE:
  
    why3find extract [OPTIONS] MODULE...
  
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
  
    Executes why3 extract with the specified arguments.
  
  OPTIONS:
  
    -v|--verbose print why3 command
    -p|--package PKG package dependency
    -D|--driver NAME|FILE additional extraction driver
    --extra-config FILE additional configuration file
  

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
    --prover PRV add automated prover
    --transf TRANS add transformation
    --driver DRV add extraction driver
    --remove remove all specified packages, proversand transformations
    -p  same as --package
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
    -help  Display this list of options
    --help  Display this list of options
  

  $ why3find prove --help
  USAGE:
  
    why3find prove [OPTIONS] PATH...
  
  DESCRIPTION:
  
    Prove all why3 files and directories accessible from PATH.
  
  OPTIONS:
  
    --root DIR change to directory
    --extra-config CFG extra why3 config
    --package PKG add package dependency
    --prover PRV add automated prover
    --transf TRANS add transformation
    --driver DRV add extraction driver
    --remove remove all specified packages, proversand transformations
    -p  same as --package
    -P  same as --prover
    -T  same as --transf
    -D  same as --driver
    -c force cache update
    -j JOBS max parallel provers
    -t TIME prover time (default 1.0s)
    -f force rebuild proofs
    -u update proofs (default)
    -r check proofs (no update)
    -m minimize proofs (or update)
    -i run why-3 IDE on error (implies -s)
    -s save why3 session
    --modules list results by module
    --theories list results by theory
    --goals list results by goals
    --proofs list proofs by goals
    --local no calibration (local times)
    -help  Display this list of options
    --help  Display this list of options
  

  $ why3find doc --help
  USAGE:
  
    why3find doc PATH...
  
  DESCRIPTION:
  
    Generate HTML documentation.
  
    Includes all why3 files and directories accessible from PATH.
  
  
  OPTIONS:
  
    --root DIR change to directory
    --extra-config CFG extra why3 config
    --package PKG add package dependency
    --driver DRV add extraction driver
    --remove remove all specified packages, proversand transformations
    -p  same as --package
    -D  same as --driver
    -o destination directory (default "html")
    -help  Display this list of options
    --help  Display this list of options
  

  $ why3find install --help
  USAGE:
  
    why3find install [OPTIONS] PKG PATH...
  
  DESCRIPTION:
  
  
    Install the package PKG at the topmost installation site.
  
    Package dependencies and configuration are taken from the
    local project, or from the command line:
  
      **/*.cfg extra why3 configuration
      **/*.drv OCaml extraction drivers
      DIR all why3 source files in DIR
      PKG/**/*.mlw why3 source files
  
    If no source file is given, all why3 source files
    in directory PKG will be installed.
  
    Unless --no-doc is specified, documentation in './html'
    directory is also installed.
  
  OPTIONS:
  
    --dune Generate dune installer (default)
    --shared Install in shared repository (why3find shared)
    --doc DIR Doc output directory (why3find doc -o DIR)
    --no-doc Do not install documentation
    -help  Display this list of options
    --help  Display this list of options
  

  $ why3find uninstall --help
  USAGE:
  
    why3find uninstall [PKG...]
  
  DESCRIPTION:
  
    Remove all specified packages from topmost installation site.
  

  $ why3find CMD --help
  USAGE:
  
    why3find CMD [ARGS...]
  
  DESCRIPTION:
  
    Execute command "CMD" with wrapped arguments.
  
  OPTIONS:
  
    --p|--package PKG : pass --library=<path> for the package
    --configs : pass also --extra-config-file=<CFG> options
    --drivers : pass also --driver=<DRV> options
  
