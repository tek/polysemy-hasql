{
  description = "Polysemy effects for databases";

  inputs = {
    hix.url = "git+https://git.tryp.io/tek/hix";
    prelate.url = "git+https://git.tryp.io/tek/prelate";
    sqel.url = "git+https://git.tryp.io/tek/sqel";
    exon.url = "git+https://git.tryp.io/tek/exon";
  };

  outputs = { hix, prelate, sqel, exon, ... }: hix.lib.pro ({config, ...}: {
    hackage.versionFile = "ops/version.nix";
    depsFull = [sqel prelate];
    main = "polysemy-hasql-test";
    compiler = "ghc94";
    gen-overrides.enable = true;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.6";
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
      dependencies = ["polysemy ^>= 1.9" "polysemy-plugin ^>= 0.4.5"];
      ghc-options = ["-fplugin=Polysemy.Plugin" "-fprint-potential-instances"];
      default-extensions = ["QualifiedDo"];
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
            "hasql ^>= 1.6"
            "postgresql-libpq ^>= 0.9"
            "sqel ^>= 0.0.1"
            "sqel-core ^>= 0.0.1"
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
            "hedgehog >= 1.1 && < 1.3"
            "path ^>= 0.9"
            "sqel ^>= 0.0.1"
            "uuid ^>= 1.3"
            "zeugma ^>= 0.8"
            config.packages.polysemy-db.dep.minor
            config.packages.polysemy-hasql.dep.minor
          ];
        };
        tests.polysemy-hasql-test-integration = {
          source-dirs = "integration";
          dependencies = [
            "aeson >= 2.0 && < 2.2"
            "exon ^>= 1.4"
            "hasql ^>= 1.6"
            "sqel ^>= 0.0.1"
            "tasty ^>= 1.4"
            "vector ^>= 0.12"
            "zeugma ^>= 0.8"
            config.packages.polysemy-db.dep.minor
            config.packages.polysemy-hasql.dep.minor
          ];
          env = "polysemy-db-integration";
        };
      };
    };

    envs.polysemy-db-integration = {
      systems = ["x86_64-linux"];
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
        polysemy_db_test_port = config.envs.polysemy-db-integration.hostPorts.postgres;
      };
    };
  });
}
