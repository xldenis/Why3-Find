{ buildDunePackage,
  gitignoreSource,
  fetchurl,
  dune_3 }:

buildDunePackage {
  pname = "ppxlib";
  version = "0.28.0";
  duneVersion = "3";

  src = fetchurl {
    url = "https://github.com/ocaml-ppx/ppxlib/releases/download/0.28.0/ppxlib-0.28.0.tbz" ;
    sha256 = "d87ae5f9a081206308ca964809b50a66aeb8e83d254801e8b9675448b60cf377" ;
  };

  buildInputs = [ dune_3 ];
}
