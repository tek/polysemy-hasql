{
  base,
}:
let
  niv = import "${toString base}/nix/sources.nix";
  nixpkgsSrc = niv.nixpkgs;
  # hsSrc = ../../../../nix/tryp-hs;
  hsSrc = niv.tryp-hs;

  nixpkgs = import nixpkgsSrc;
  hs = import hsSrc { inherit base; };

  packages = {
    polysemy-db = base + /packages/db;
    polysemy-hasql = base + /packages/hasql;
  };

  project = hs.project {
    inherit nixpkgs packages base;
    compiler = "ghc884";
    cabal2nixOptions = "--no-hpack";
    overrides = import ./overrides.nix niv;
    ghci = {
      basicArgs = [
        "-Werror"
        "-fmax-relevant-binds=1"
        "-fmax-valid-hole-fits=1"
        "-fprint-potential-instances"
      ];
      options_ghc = "-fplugin=Polysemy.Plugin";
    };
    ghcid = {
      prelude = base + /packages/db/lib/Prelude;
    };
    packageDir = "packages";
  };
in
  project
