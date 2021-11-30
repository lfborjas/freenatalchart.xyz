let
  pkgs = import ./packages.nix {};
in
  { freenatalchart = pkgs.haskellPackages.freenatalchart; }
