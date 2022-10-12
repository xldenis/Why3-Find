{ buildDunePackage,
  gitignoreSource,
  dune_3,
  dune-site,
  why3,
  yojson }:

buildDunePackage {
  pname = "why3find";
  version = "dev";

  src = gitignoreSource ./..;

  doCheck = true;

  buildInputs = [ dune_3 dune-site why3 yojson ];
}
