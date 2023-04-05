{ buildDunePackage,
  gitignoreSource,
  dune_3,
  dune-site,
  alt-ergo,
  menhir,
  menhirLib,
  mlmpfr,
  why3,
  zmq,
  zeromq,
  yojson,
  terminal_size }:

buildDunePackage {
  pname = "why3find";
  version = "dev";
  duneVersion = "3";

  src = gitignoreSource ./..;

  preBuild = ''
    mkdir home
    export HOME=$(pwd)/home
    why3 config detect
  '';

  doCheck = true;

  buildInputs = [
    dune_3
    dune-site
    alt-ergo
    mlmpfr
    menhir
    menhirLib
    why3
    zeromq
    zmq
    yojson
    terminal_size
  ];
}
