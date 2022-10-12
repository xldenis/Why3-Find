{ buildDunePackage,
  gitignoreSource,
  dune_3,
  dune-site,
  alt-ergo,
  why3,
  yojson }:

buildDunePackage {
  pname = "why3find";
  version = "dev";
  duneVersion = "3";

  src = gitignoreSource ./..;

  doCheck = true;

  buildInputs = [ dune_3 dune-site alt-ergo why3 yojson ];
}
