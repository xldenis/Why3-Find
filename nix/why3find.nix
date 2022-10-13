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
  preCheck = ''
    patchShebangs .
    mkdir home
    HOME=$(pwd)/home
    why3 config detect
    export FRAMAC_WP_CACHE=offline
    export FRAMAC_WP_CACHEDIR=$wp_cache
  '';

  buildInputs = [ dune_3 dune-site alt-ergo why3 yojson ];
}
