let
  sources = import ./sources.nix {};
  ocamlOverlay = oself: osuper: {
    why3 = oself.callPackage ./why3.nix {};
    why3find = oself.callPackage ./why3find.nix {};
  };
  overlay = self: super: {
    ocaml-ng = super.lib.mapAttrs (
      name: value:
        if builtins.hasAttr "overrideScope'" value
        then value.overrideScope' ocamlOverlay
        else value
    ) super.ocaml-ng;
    inherit (super.callPackage sources."gitignore.nix" {}) gitignoreSource;
    why3 = throw "don't use pkgs.why3 but ocaml-ng.ocamlPackages_4_XX.why3";
    why3find = self.ocamlPackages.why3find;
  };
  pkgs = import sources.nixpkgs {
    overlays = [ overlay ];
  };
in
pkgs
