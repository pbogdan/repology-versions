let
  repology-versions = (import ./default.nix {});
  sources = import ./nix/sources.nix;
  pkgs = (import sources.unstable {});
in
pkgs.haskellPackages.shellFor {
  packages = _: [
    repology-versions
  ];
  withHoogle = false;
}
