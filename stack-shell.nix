{ghc}:
with (import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz") {});

haskell.lib.buildStackProject {
  name = "env";
  buildInputs = [ proj ];
}
