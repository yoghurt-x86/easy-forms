{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
    pkgs.inotify-tools
    pkgs.nodejs-16_x
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-format
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-review
    pkgs.elmPackages.elm-language-server
    pkgs.elmPackages.elm-live
    pkgs.esbuild
  ];
  shellHook = ''
    export LANG=en_US.UTF-8
  '';
}
