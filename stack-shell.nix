let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-24.11.tar.gz";
    sha256 = "sha256:0hdb0ajwfl7xn1hznik4kj35adiksc8k5apllz3jniwrszpkwrwm";
  };
  pkgs = import nixpkgs {};
in
{ghc}:
with pkgs;

haskell.lib.buildStackProject {
  name = "env";
  buildInputs = [ bzip2 zlib snappy stdenv.cc.cc.lib proj ];
}
