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
    version = "0.1.0.0";
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
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
    { name = "prelate"; version = ">= 0.4"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
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

in {

  polysemy-db = merge (polysemy "polysemy-db" "Polysemy-Db") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "exon"
      "first-class-families"
      "generics-sop"
      "lens"
      "random"
      "sqel"
      "time"
      "type-errors"
      "uuid"
    ];
  };

  polysemy-hasql = merge (polysemy "polysemy-hasql" "Polysemy-Hasql") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "async"
      "containers"
      "exon"
      "extra"
      "generics-sop"
      "hasql >= 1.4.3"
      "polysemy-conc"
      "polysemy-db"
      "polysemy-log"
      "polysemy-time"
      "postgresql-libpq"
      "prettyprinter"
      "sqel"
      "stm-chans"
      "torsor"
      "transformers"
      "uuid"
    ];

    tests.polysemy-hasql-unit = polysemyExe "polysemy-hasql" "test" {
      dependencies = [
        "aeson"
        "chronos"
        "exon"
        "first-class-families"
        "generics-sop"
        "hasql >= 1.4.3"
        "path"
        "polysemy-db"
        "polysemy-hasql"
        "polysemy-test"
        "sqel"
        "tasty"
      ];
    };

    tests.polysemy-hasql-integration = polysemyExe "polysemy-hasql" "integration" {
      dependencies = [
        "aeson"
        "exon"
        "first-class-families"
        "generics-sop"
        "hasql >= 1.4.3"
        "hedgehog"
        "lens"
        "path"
        "polysemy-conc"
        "polysemy-db"
        "polysemy-hasql"
        "polysemy-log"
        "polysemy-test"
        "polysemy-time"
        "prettyprinter"
        "sqel"
        "tasty"
        "time"
        "uuid"
        "vector"
      ];
    };

  };

  polysemy-hasql-test = merge (polysemy "polysemy-hasql-test" "Polysemy-Hasql-Test") {
    synopsis = "Test utilities for polysemy-hasql";

    library.dependencies = [
      "hasql >= 1.4.3"
      "hedgehog"
      "path"
      "polysemy-test"
      "polysemy-db"
      "polysemy-hasql"
      "sqel"
    ];

    tests.polysemy-hasql-test-unit = polysemyExe "polysemy-hasql-test" "test" {
      dependencies = [
        "aeson"
        "chronos"
        "exon"
        "first-class-families"
        "generics-sop"
        "hasql >= 1.4.3"
        "path"
        "polysemy-db"
        "polysemy-hasql"
        "polysemy-hasql-test"
        "polysemy-test"
        "sqel"
        "tasty"
      ];
    };

  };

  sqel = merge (project "sqel" "Sqel") {
    synopsis = "Guided derivation for Hasql statements";
    library.dependencies = [
      "aeson"
      "chronos"
      "composition"
      "containers"
      "contravariant"
      "exon"
      "extra"
      "first-class-families"
      "generics-sop"
      "hasql >= 1.4.3"
      "invariant"
      "path"
      "path-io"
      "prettyprinter"
      "scientific"
      "singletons"
      "some"
      "template-haskell"
      "time"
      "transformers"
      "type-errors"
      "uuid"
      "vector"
    ];

    tests.polysemy-hasql-unit = polysemyExe "sqel" "test" {
      dependencies = [
        "exon"
        "generics-sop"
        "hedgehog"
        "path"
        "tasty"
        "tasty-hedgehog"
      ];
    };

  };


}
