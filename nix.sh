## you have to regenerate default.nix every time you update the .cabal file.
nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

