{ buildDunePackage,
  gitignoreSource,
  why3,
  yojson }:

buildDunePackage {
  pname = "why3find";
  version = "dev";

  src = gitignoreSource ./..;

  doCheck = true;

  buildInputs = [ why3 yojson ];
}
