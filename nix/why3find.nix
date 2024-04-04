{ buildDunePackage,
  gitignoreSource,
  dune_3,
  dune-site,
  headache,
  ocp-indent,
  alt-ergo,
  bisect_ppx,
  menhir,
  menhirLib,
  mlmpfr,
  ppxlib,
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
    export DUNE_INSTRUMENT_WITH=meta_bisect_ppx
  '';

  nativeBuildInputs = [ why3 ] ;

  buildInputs = [
    dune_3
    dune-site
    alt-ergo
    bisect_ppx
    mlmpfr
    menhir
    menhirLib
    ppxlib
    why3
    zeromq
    zmq
    yojson
    terminal_size
  ];

  doCheck = true;

  nativeCheckInputs = [ headache ocp-indent alt-ergo bisect_ppx ] ;

  checkPhase = ''
    BISECT_FILE=$(pwd)/bisect \
    dune runtest -p why3find -j1 --force --instrument-with bisect_ppx
    bisect-ppx-report cobertura report.xml
    bisect-ppx-report summary | sed -e 's/.*(\(1\?[0-9]\{2\}\.[0-9]\+%\))/Coverage: \1/' > coverage.txt

    # Check indentation and headers
    ./nix/check.sh
  '' ;

  postInstall = ''
    cp report.xml $out
    cp coverage.txt $out
  '';
}
