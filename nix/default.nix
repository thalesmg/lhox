{ sources ? import ./sources.nix
}:

let
  pkgs = import sources.nixpkgs {};
  hsPkgs' = pkgs.haskellPackages;
  gitignoreSource = (import sources."gitignore.nix" { inherit (pkgs) lib; }).gitignoreSource;
  hsPkgs = hsPkgs'.extend (pkgs.haskell.lib.packageSourceOverrides {
    lhox = gitignoreSource ./..;
  });
in
{
  inherit pkgs hsPkgs gitignoreSource;

  inherit (hsPkgs) lhox;

  devTools = {
    inherit (pkgs) niv;
  };
}
