{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else throw "foreach: expected list or attrset but got ${typeOf xs}"
      );
      sourceFilter = root: with lib.fileset; toSource {
        inherit root;
        fileset = fileFilter
          (file: builtins.any file.hasExt [ "cabal" "hs" "md" ] )
          root;
      };
      pname = "rhine-nix";
      overlay = lib.composeManyExtensions [
        (final: prev: {
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions [
              prev.haskell.packageOverrides
              (hfinal: hprev: {
                ${pname} = hfinal.callCabal2nix pname (sourceFilter ./.) { };
              })
            ];
          };
        })
      ];
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs':
      let
        pkgs = pkgs'.extend overlay;
      in
      {
        packages.${system}.default = pkgs.haskellPackages.${pname};

        devShells.${system}.default = pkgs.haskellPackages.shellFor {
          packages = hp: [ hp.${pname} ];
          nativeBuildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
          ];
        };
      });
}
