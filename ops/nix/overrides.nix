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
    (pack "binary-parser" "0.5.6" "0xh2fqygw4qqvac5938431kyvks3bdxrhyyl29iynhlkjcgyclqv")
    (pack "first-class-families" "0.8.0.0" "0266lqagnxmd80n9i0f1xsh4zfrmab5aazyp4ii5nqch3474gpm6")
    (pack "hashable" "1.3.0.0" "10w1a9175zxy11awir48axanyy96llihk1dnfgypn9qwdnqd9xnx")
    (pack "hasql" "1.4.4.2" "1p70n4a7980bv467yrkrr832zd2aalnq810bl6fv071a93gkzc4f")
    (pack "postgresql-binary" "0.12.3.1" "1qqd5rln67lkff7dxkar1d4p5ggd59kqcv15la2d2i0pdxmbkhkl")
    (pack "relude" "0.7.0.0" "0flrwzxdd9bd3knk48zkhadwlad01msskjby1bfv4snr44q5xfqd")
  ];
  versionOverrides = builtins.listToAttrs versions;

  custom = {
    polysemy = cabal2nix "polysemy" niv.polysemy;
    polysemy-plugin = subPkg "polysemy-plugin" "polysemy-plugin" niv.polysemy;
    polysemy-test = subPkg "packages/polysemy-test" "polysemy-test" niv.polysemy-test;
  };
in
  versionOverrides // custom
