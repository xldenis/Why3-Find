let
  sources = import ./sources.nix {};
  ocamlOverlay = oself: osuper: {
    camlzip = oself.callPackage ./camlzip.nix {};
    menhirLib = oself.callPackage ./menhirLib.nix {};
    mlmpfr = oself.callPackage ./mlmpfr.nix {};
    psmt2-frontend = oself.callPackage ./psmt2-frontend.nix {};
    alt-ergo = oself.callPackage ./alt-ergo.nix {};
    why3 = oself.callPackage ./why3.nix {};
    why3find = oself.callPackage ./why3find.nix {};
  };
  overlay = self: super: {
    niv = (import sources.niv {}).niv;
    ocaml-ng = super.lib.mapAttrs (
      name: value:
        if builtins.hasAttr "overrideScope'" value
        then value.overrideScope' ocamlOverlay
        else value
    ) super.ocaml-ng;
    inherit (super.callPackage sources."gitignore.nix" {}) gitignoreSource;
    camlzip = throw "don't use pkgs.camlzip but ocaml-ng.ocamlPackages_4_XX.camlzip";
    why3 = throw "don't use pkgs.why3 but ocaml-ng.ocamlPackages_4_XX.why3";
    why3find = self.ocamlPackages.why3find;
  };
  pkgs = import sources.nixpkgs {
    # alt-ergo
    config.allowUnfree = true;
    overlays = [ overlay ];
  };
in
pkgs
