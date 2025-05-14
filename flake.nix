{
  description = "Noxia Compiler Dev Environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShell = pkgs.mkShell {
          name = "noxia-dev";

          buildInputs = [
            pkgs.python3
            pkgs.python3Packages.setuptools
            pkgs.python3Packages.wheel
            pkgs.nasm
            pkgs.clang
            pkgs.lld
            pkgs.nodejs_20
            pkgs.yarn
            pkgs.vsce
          ];

          shellHook = ''
            echo "🚀 Noxia dev shell ready. Use \`python3 main.py\` to build."
          '';
        };
      });
}

