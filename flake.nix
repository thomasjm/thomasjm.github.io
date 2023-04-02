{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: let
      pkgs = import nixpkgs { inherit system; };

      ruby = pkgs.ruby_3_1;

      env = pkgs.bundlerEnv {
        name = "your-package";
        inherit ruby;
        gemfile = ./Gemfile;
        lockfile = ./Gemfile.lock;
        gemset = ./gemset.nix;
      };

      serveScript = pkgs.writeShellScript "jekyll-serve.sh" ''
        ${env}/bin/jekyll serve
      '';

    in
      rec {
        apps = {
          default = {
            type = "app";
            program = "${serveScript}";
          };
        };

        devShells = rec {
          shellNix = import ./shell.nix { inherit pkgs; };
          jekyll = pkgs.mkShell {
            name = "jekyll-env";
            packages = [env pkgs.bundler ruby];
          };
          default = jekyll;
        };
      }
    );
}
