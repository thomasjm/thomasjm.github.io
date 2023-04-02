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

      buildScript = pkgs.writeShellScript "jekyll-build.sh" ''
        ${env}/bin/jekyll build
      '';

      bundixScript = pkgs.writeShellScript "jekyll-bundix.sh" ''
        ${pkgs.bundix}/bin/bundix -l
      '';

    in
      rec {
        apps = rec {
          serve = {
            type = "app";
            program = "${serveScript}";
          };

          build = {
            type = "app";
            program = "${buildScript}";
          };

          bundix = {
            type = "app";
            program = "${bundixScript}";
          };

          default = serve;
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
