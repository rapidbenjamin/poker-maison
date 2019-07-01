# so we can access the `pkgs` and `stdenv` variables
with import <nixpkgs> {};

# Make a new "derivation" that represents our shell
stdenv.mkDerivation {
  name = "my-environment";

  # The packages in the `buildInputs` list will be added to the PATH in our shell
  buildInputs = [
    pkgs.nodejs-10_x
    pkgs.yarn
    pkgs.ocaml
    pkgs.pngquant
    pkgs.libpng12
    pkgs.python
    pkgs.autoreconfHook
  ];
}
