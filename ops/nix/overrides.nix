niv:
{
  pkgs,
  hackage,
}:
self: super:
let
  inherit (hackage) pack thunk cabal2nix subPkg github;

  versions = [
    (pack "base-prelude" "1.4" "1yg8r5qsl0qli4ifall9qrm0xarvhvb5z69i92laf2rmnarkny1c")
    (pack "hasql" "1.4.4.2" "1p70n4a7980bv467yrkrr832zd2aalnq810bl6fv071a93gkzc4f")
    (pack "postgresql-binary" "0.12.3.1" "1qqd5rln67lkff7dxkar1d4p5ggd59kqcv15la2d2i0pdxmbkhkl")
  ];
  versionOverrides = builtins.listToAttrs versions;

  custom = {
    polysemy = cabal2nix "polysemy" niv.polysemy;
    polysemy-plugin = subPkg "polysemy-plugin" "polysemy-plugin" niv.polysemy;
    polysemy-test = subPkg "packages/polysemy-test" "polysemy-test" niv.polysemy-test;
    polysemy-time = subPkg "packages/time" "polysemy-time" niv.polysemy-time;
  };
in
  versionOverrides // custom
