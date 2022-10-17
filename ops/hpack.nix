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
    { name = "prelate"; version = ">= 0.2"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
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
    dependencies = dependencies ++ ["polysemy" "polysemy-plugin" pkg];
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  polysemy-db-data = merge (project "polysemy-db-data" "Polysemy-Db-Data") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "aeson"
      "comonad"
      "data-default"
      "lens"
      "template-haskell"
      "uuid"
    ];
  };

  polysemy-db = merge (polysemy "polysemy-db" "Polysemy-Db") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "aeson"
      "composition"
      "exon"
      "first-class-families"
      "generics-sop"
      "lens"
      "path"
      "polysemy-db-data"
      "random"
      "time"
      "type-errors"
      "type-errors-pretty"
      "uuid"
      "vector"
    ];
  };

  polysemy-hasql = merge (polysemy "polysemy-hasql" "Polysemy-Hasql") {
    synopsis = "Polysemy effects for databases";
    library.dependencies = [
      "aeson"
      "async"
      "chronos"
      "composition"
      "contravariant"
      "exon"
      "fcf-containers >= 0.6"
      "first-class-families"
      "generics-sop"
      "hasql >= 1.4.3"
      "hasql-dynamic-statements"
      "lens"
      "path"
      "polysemy-conc"
      "polysemy-db"
      "polysemy-db-data"
      "polysemy-log"
      "polysemy-time"
      "postgresql-libpq"
      "scientific"
      "stm-chans"
      "template-haskell"
      "time"
      "torsor"
      "transformers"
      "type-errors"
      "type-errors-pretty"
      "uuid"
      "vector"
    ];

    tests.polysemy-hasql-unit = exe "polysemy-hasql" "test" {
      dependencies = [
        "aeson"
        "chronos"
        "exon"
        "first-class-families"
        "generics-sop"
        "hasql >= 1.4.3"
        "path"
        "polysemy-db"
        "polysemy-db-data"
        "polysemy-hasql"
        "polysemy-plugin"
        "polysemy-test"
        "tasty"
        "uuid"
      ];
    };

    tests.polysemy-hasql-integration = exe "polysemy-hasql" "integration" {
      dependencies = [
        "aeson"
        "exon"
        "hasql >= 1.4.3"
        "hedgehog"
        "lens"
        "path"
        "polysemy-conc"
        "polysemy-db"
        "polysemy-db-data"
        "polysemy-hasql"
        "polysemy-log"
        "polysemy-plugin"
        "polysemy-test"
        "polysemy-time"
        "tasty"
        "time"
        "uuid"
      ];
    };

  };

}
