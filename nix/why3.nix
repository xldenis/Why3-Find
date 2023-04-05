{ callPackage
, fetchurl
, fetchpatch
, lib
, stdenv
, ocaml
, findlib
, ocamlgraph
, zarith
, menhir
, menhirLib
, mlmpfr
, js_of_ocaml
, js_of_ocaml-ppx
, ppx_deriving
, ppx_sexp_conv
, camlzip
, sexplib
, re
, num
, lablgtk3-sourceview3
, coqPackages
, rubber
, hevea
, emacs
, autoreconfHook
}:

stdenv.mkDerivation rec {
  pname = "why3";
  version = src.version;

  src = (import ./sources.nix {}).why3;
  nativeBuildInputs = [
    autoreconfHook
  ];
  buildInputs = [
    ocaml
    findlib
    ocamlgraph
    zarith
    menhir
    menhirLib
    mlmpfr
    # Emacs compilation of why3.el
    emacs
    # Documentation
    rubber
    hevea
    # GUI
    lablgtk3-sourceview3
    # WebIDE
    js_of_ocaml
    js_of_ocaml-ppx
    # S-expression output for why3pp
    ppx_deriving
    ppx_sexp_conv
    # Coq Support
    coqPackages.coq
    coqPackages.flocq
  ];

  propagatedBuildInputs = [ camlzip num re sexplib ];

  enableParallelBuilding = true;

  configureFlags = [ "--enable-verbose-make" ];

  installTargets = [ "install" "install-lib" ];

  meta = with lib; {
    description = "A platform for deductive program verification";
    homepage = "http://why3.lri.fr/";
    license = licenses.lgpl21;
    platforms = platforms.unix;
    maintainers = with maintainers; [ thoughtpolice vbgl ];
  };
}
