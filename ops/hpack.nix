{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = import ./version.nix;
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2023 Torsten Schmits";
    category = "Database";
    build-type = "Simple";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
  ];

  dependencies = [
    { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
    { name = "prelate"; version = "^>= 0.5.1"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
  ];

  project = name: doc: merge (meta // { library = paths name; } // options) {
    inherit name;
    description = "See https://hackage.haskell.org/package/${name}/docs/${doc}.html";
    library = {
      source-dirs = "lib";
      inherit dependencies;
    };
    default-extensions = config.ghci.extensions;
  };

  polysemy = name: doc: merge (project name doc) {
    library.dependencies = ["polysemy" "polysemy-plugin"];
    ghc-options = ["-fplugin=Polysemy.Plugin"];
  };

  exe = pkg: dir: merge (paths pkg // {
    main = "Main.hs";
    source-dirs = dir;
    dependencies = dependencies ++ [pkg];
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

  polysemyExe = pkg: dir: merge (exe pkg dir {
    dependencies = ["polysemy" "polysemy-plugin"];
  });

  dep = n: "${n} ^>= ${import ./version.nix}";
  dep_db = dep "polysemy-db";
  dep_hasql = dep "polysemy-hasql";

in {

  polysemy-db = merge (polysemy "polysemy-db" "Polysemy-Db") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "exon ^>= 1.4"
      "random ^>= 1.2"
      "sqel ^>= 0.0.1"
      "uuid ^>= 1.3"
    ];
  };

  polysemy-hasql = merge (polysemy "polysemy-hasql" "Polysemy-Hasql") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "async ^>= 2.2"
      "containers"
      "exon ^>= 1.4"
      "generics-sop ^>= 0.5"
      "hasql ^>= 1.6"
      dep_db
      "postgresql-libpq ^>= 0.9"
      "sqel ^>= 0.0.1"
      "stm-chans ^>= 3.0"
      "torsor ^>= 0.1"
      "transformers"
      "uuid ^>= 1.3"
    ];

    tests.polysemy-hasql-integration = polysemyExe "polysemy-hasql" "integration" {
      dependencies = [
        "aeson ^>= 2.0"
        "exon ^>= 1.4"
        "generics-sop ^>= 0.5"
        "hasql ^>= 1.6"
        "hedgehog ^>= 1.1"
        "polysemy-test ^>= 0.7"
        "tasty ^>= 1.4"
        "uuid ^>= 1.3"
        "vector ^>= 0.12"
        "zeugma ^>= 0.7"
        dep_db
        dep_hasql
        "sqel ^>= 0.0.1"
      ];
    };

  };

  polysemy-hasql-test = merge (polysemy "polysemy-hasql-test" "Polysemy-Hasql-Test") {
    synopsis = "Test utilities for polysemy-hasql";

    library.dependencies = [
      "hasql ^>= 1.6"
      "hedgehog ^>= 1.1"
      "path ^>= 0.9"
      "polysemy-test ^>= 0.7"
      "uuid ^>= 1.3"
      dep_db
      dep_hasql
      "sqel ^>= 0.0.1"
    ];

    tests.polysemy-hasql-test-unit = polysemyExe "polysemy-hasql-test" "test" {
      dependencies = [
        "path ^>= 0.9"
        "polysemy-test ^>= 0.7"
        "tasty ^>= 1.4"
        "sqel ^>= 0.0.1"
      ];

    };

  };

}
