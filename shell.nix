{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    nixpkgs-fmt

    bundler
    bundix
  ];

  shellHook = ''
    # ...
  '';
}
