niv:
{
  pkgs,
  hackage,
}:
self: super:
let
  inherit (hackage) pack jpack thunk cabal2nix subPkg github;

  versions = [
    (pack "base-prelude" "1.4" "1yg8r5qsl0qli4ifall9qrm0xarvhvb5z69i92laf2rmnarkny1c")
    (jpack "fcf-containers" "0.5.0" "13aj2w9zvsmsxj09hqgaj38bb7n0npw6zigw7mdlkv94zwc43ydy")
    (pack "hasql" "1.4.4.2" "1p70n4a7980bv467yrkrr832zd2aalnq810bl6fv071a93gkzc4f")
    (pack "path" "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb")
    (pack "path-io" "0.3.1" "07m7q36pdkqk18bmf0lkafjc9npksym7dhn2am1m9c1rvj3b26qf")
    (pack "postgresql-binary" "0.12.3.1" "1qqd5rln67lkff7dxkar1d4p5ggd59kqcv15la2d2i0pdxmbkhkl")
  ];
  versionOverrides = builtins.listToAttrs versions;

  custom = {
    polysemy = cabal2nix "polysemy" niv.polysemy;
    polysemy-plugin = subPkg "polysemy-plugin" "polysemy-plugin" niv.polysemy;
    polysemy-resume = subPkg "packages/resume" "polysemy-resume" niv.polysemy-resume;
    polysemy-test = subPkg "packages/polysemy-test" "polysemy-test" niv.polysemy-test;
    polysemy-time = subPkg "packages/time" "polysemy-time" niv.polysemy-time;
  };
in
  versionOverrides // custom
