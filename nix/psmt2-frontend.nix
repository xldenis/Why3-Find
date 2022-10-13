{ callPackage
, fetchFromGitHub
, lib
, stdenv
, ocaml
, findlib
, menhir
, autoreconfHook
, which
}:

stdenv.mkDerivation rec {
  pname = "psmt2-frontend";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "Coquera";
    repo = pname;
    rev = version;
    sha256 = "0k7jlsbkdyg7hafmvynp0ik8xk7mfr00wz27vxn4ncnmp20yz4vn";
  };

  nativeBuildInputs = [
    autoreconfHook
    which
  ];

  buildInputs = [
    ocaml
    findlib
    menhir
  ];

  enableParallelBuilding = true;

  configureFlags = [ "--enable-verbose-make" ];

  createFindlibDestdir = true;

  installFlags = "LIBDIR=$(OCAMLFIND_DESTDIR)";

  meta = {
    description = "A simple parser and type-checker for polomorphic extension of the SMT-LIB 2 language";
    license = lib.licenses.asl20;
    maintainers = [ lib.maintainers.vbgl ];
    inherit (src.meta) homepage;
  };
}
