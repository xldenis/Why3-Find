{ buildDunePackage,
  gitignoreSource,
  dune_3,
  dune-site,
  alt-ergo,
  why3,
  zmq,
  zeromq,
  yojson }:

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

  buildInputs = [ dune_3 dune-site alt-ergo why3 zeromq zmq yojson ];
}
