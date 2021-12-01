let
    nixpkgs = import ./nix/packages.nix {};
    lib = import ./nix/release.nix;

in

    nixpkgs.haskellPackages.shellFor {
        packages = p: builtins.attrValues lib;
        buildInputs = [
            nixpkgs.haskellPackages.cabal-install
            nixpkgs.haskellPackages.haskell-language-server
            nixpkgs.tzdata
        ];
        shellHook = ''
        if [ ! -d "/usr/share/zoneinfo" ]; then
            echo "NO ZONEINFO, LINKING";
            ln -sf ${nixpkgs.tzdata}/share/zoneinfo /usr/share/zoneinfo
        else
            echo "ZONEINFO OK";
        fi
        '';
    }
