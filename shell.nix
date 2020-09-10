let
  main = import ./.;
in
  main.ghcid.shell // {
    hls = true;
    inherit (main) ghcid cabal;
  }
