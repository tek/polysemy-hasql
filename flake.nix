{
  description = "Polysemy effects for databases";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    hls.url = "github:haskell/haskell-language-server?ref=1.9.0.0";
    prelate.url = "git+https://git.tryp.io/tek/prelate";
    sqel.url = "git+https://git.tryp.io/tek/sqel";
  };

  outputs = { hix, hls, prelate, sqel, ... }: hix.lib.pro ({config, ...}: {
    hackage.versionFile = "ops/version.nix";
    depsFull = [sqel prelate];
    main = "polysemy-hasql-test";
    ifd = false;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.5.1";
        };
        module = "Prelate";
      };
      paths = false;
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Database";
        git = "https://git.tryp.io/tek/polysemy-hasql";
        homepage = "https://git.tryp.io/tek/polysemy-hasql";
        bug-reports = "https://github.com/tek/polysemy-hasql/issues";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
      ghc-options = ["-fplugin=Polysemy.Plugin" "-Wno-partial-type-signatures" "-fprint-potential-instances"];
    };

    packages = {
      polysemy-db = {
        src = ./packages/db;
        cabal.meta.synopsis = "Polysemy effects for databases";
        library = {
          enable = true;
          dependencies = [
            "exon ^>= 1.4"
            "microlens ^>= 0.4"
            "random ^>= 1.2"
            "sqel ^>= 0.0.1"
            "uuid ^>= 1.3"
          ];
        };
      };

      polysemy-hasql = {
        src = ./packages/hasql;
        cabal.meta.synopsis = "Polysemy effects for Hasql databases";
        library = {
          enable = true;
          dependencies = [
            "async ^>= 2.2"
            "containers"
            "exon ^>= 1.4"
            "generics-sop ^>= 0.5"
            "hasql ^>= 1.6"
            "postgresql-libpq ^>= 0.9"
            "sqel ^>= 0.0.1"
            "stm-chans ^>= 3.0"
            "torsor ^>= 0.1"
            "transformers"
            "uuid ^>= 1.3"
            config.packages.polysemy-db.dep.minor
          ];
        };
      };

      polysemy-hasql-test = {
        src = ./packages/hasql-test;
        cabal.meta.synopsis = "Test utilities for polysemy-hasql";
        library = {
          enable = true;
          dependencies = [
            "hasql ^>= 1.6"
            "hedgehog ^>= 1.1"
            "path ^>= 0.9"
            "sqel ^>= 0.0.1"
            "uuid ^>= 1.3"
            "zeugma ^>= 0.7"
            config.packages.polysemy-db.dep.minor
            config.packages.polysemy-hasql.dep.minor
          ];
        };
        test = {
          enable = true;
          dependencies = [
            "path ^>= 0.9"
            "polysemy-test ^>= 0.7"
            "tasty ^>= 1.4"
            "sqel ^>= 0.0.1"
          ];
        };
        tests.polysemy-hasql-test-integration = {
          source-dirs = "integration";
          dependencies = [
            "aeson ^>= 2.0"
            "exon ^>= 1.4"
            "generics-sop ^>= 0.5"
            "hasql ^>= 1.6"
            "hedgehog ^>= 1.1"
            "sqel ^>= 0.0.1"
            "tasty ^>= 1.4"
            "uuid ^>= 1.3"
            "vector ^>= 0.12"
            "zeugma ^>= 0.7"
            config.packages.polysemy-db.dep.minor
            config.packages.polysemy-hasql.dep.minor
          ];
          env = "integration";
        };
      };
    };

    envs.integration = {
      services.postgres = {
        enable = true;
        config = {
          name = "polysemy_db";
          log = true;
          creds = {
            user = "polysemy_db";
            password = "polysemy_db";
          };
        };
      };

      env = {
        polysemy_db_test_host = "localhost";
        polysemy_db_test_port = config.envs.integration.hostPorts.postgres;
      };
    };

    compiler = "ghc925";
    envs.hls.hls.package = hls.packages.${config.system}.haskell-language-server-925;
  });
}
