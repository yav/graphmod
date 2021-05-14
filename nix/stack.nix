{ ghc }:

with import ./nixpkgs.nix { };

haskell.lib.buildStackProject {
  inherit ghc;
  name = "graphmod-stack-env";
  buildInputs = [ ];
}
