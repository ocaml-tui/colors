{
  description = "A pure OCaml library for manipulating colors in different color spaces";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          inherit (pkgs) ocamlPackages mkShell;
          inherit (ocamlPackages) buildDunePackage;
          name = "colors";
          version = "0.0.1";
        in
        {
          devShells = {
            default = mkShell {
	      buildInputs = [ ocamlPackages.utop ];
              inputsFrom = [ self'.packages.default ];
            };
          };

          packages = {
            default = buildDunePackage {
              inherit version;
              pname = name;
	      propagatedBuildInputs = with ocamlPackages; [
            	(mdx.override {
              	  inherit logs;
            	})
	      ];
              src = ./.;
            };
	  };
        };
    };
}
