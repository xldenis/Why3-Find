{ stdenv, lib, fetchFromGitHub, autoreconfHook, ocaml, findlib }:

let
  pname = "ocplib-simplex";
  version = "0.4.1";
in

stdenv.mkDerivation {
  name = "ocaml${ocaml.version}-${pname}-${version}";

  src = fetchFromGitHub {
    owner = "OCamlPro-Iguernlala";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-bhlTBpJg031x2lUjwuVrhQgOGmDLW/+0naN8wRjv6i4=";
  };

  nativeBuildInputs = [ autoreconfHook ocaml findlib ];

  strictDeps = true;

  installFlags = [ "LIBDIR=$(OCAMLFIND_DESTDIR)" ];

  createFindlibDestdir = true;

  meta = {
    description = "An OCaml library implementing a simplex algorithm, in a functional style, for solving systems of linear inequalities";
    homepage = "https://github.com/OCamlPro-Iguernlala/ocplib-simplex";
    inherit (ocaml.meta) platforms;
    license = lib.licenses.lgpl21;
    maintainers = [ lib.maintainers.vbgl ];
  };
}
