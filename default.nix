{ project ? import ./nix {}
}:

{
  inherit (project) lhox;
}
