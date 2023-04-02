{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: let
      pkgs = import nixpkgs { inherit system; };

      env = pkgs.bundlerEnv {
        name = "your-package";
        inherit (pkgs) ruby;
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
            packages = [env pkgs.bundler pkgs.ruby];
          };
          default = jekyll;
        };
      }
    );
}
