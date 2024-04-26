
{ lib
, fetchFromGitHub
, gmp
, mpfr
, buildDunePackage
}:

buildDunePackage rec {
  pname = "mlmpfr";
  version = "4.1.0-bugfix2";

  minimumOCamlVersion = "4.04";

  src = fetchFromGitHub {
    owner = "thvnx";
    repo = pname;
    rev = pname+"."+version;
    sha256 = "19g26jv6cjinpl5pcjif1ldyaagxlandp3qjajsy8srqg4a5rg0d";
  };

  propagatedBuildInputs = [ gmp mpfr ];

  meta = {
    description = "The package provides bindings for MPFR";
    license = lib.licenses.lgpl3Only;
    maintainers = [ ];
    homepage = "https://github.com/thvnx/mlmpfr";
  };
}
