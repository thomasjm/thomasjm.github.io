{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system: let
      pkgs = import nixpkgs { inherit system; };

      ruby = pkgs.ruby_3_1;

      env = pkgs.bundlerEnv {
        name = "thomasjm-github-io-env";
        inherit ruby;
        gemfile = ./Gemfile;
        lockfile = ./Gemfile.lock;
        gemset = ./gemset.nix;
        buildInputs = [pkgs.bintools];
        gemConfig.nokogiri = attrs: {
          version = attrs.version + "-x86_64-linux";
          buildInputs = [ pkgs.zlib ];
        };
        gemConfig.sass-embedded = attrs: {
          SASS_EMBEDDED = pkgs.fetchurl {
            url = "https://github.com/sass/dart-sass-embedded/releases/download/1.60.0/sass_embedded-1.60.0-linux-x64.tar.gz";
            sha256 = "1x85l6s3bsdawcchs1n4hryy3yl6hidypzwswzqyhjqx0f5ask0k";
          };
        };
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
      {
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

        packages = {
          default = serveScript;
        };

        devShells = rec {
          shellNix = import ./shell.nix { inherit pkgs; };
          jekyll = pkgs.mkShell {
            name = "jekyll-env";
            packages = [pkgs.bundler ruby];
          };
          default = jekyll;
        };
      }
    );
}
