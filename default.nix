{ sources ? (import ./nix/sources.nix)
, compiler ? "ghc883"
}:
let
  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages.${compiler}.override {
          overrides = hself: hsuper: with self.haskell.lib; {
          };
        };
      };
    };
  };

  pkgs = (
    import sources.unstable {
      overlays = [
        overlay
      ];
    }
  );

  inherit (pkgs.haskellPackages)
    callCabal2nix
    ;
    
  inherit (pkgs)
    nix-gitignore
    ;

  repology-versions = callCabal2nix "repology-versions" (nix-gitignore.gitignoreSource [] ./.) {};
in
  repology-versions
