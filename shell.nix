{ project ? import ./nix {}
}:

let
  inherit (project) pkgs hsPkgs lhox;
in
hsPkgs.shellFor {
  packages = p: [
    p.lhox
  ];
  buildInputs = [
    pkgs.cabal-install
  ];
}
