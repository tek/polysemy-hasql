{
  description = "Polysemy Effects for Databases";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
  };

  outputs = { hix, prelate, ... }:
  let

    all = { hackage, jailbreak, source, notest, unbreak, ... }: {
      flatparse = hackage "0.3.5.1" "0gbn93jnmj0x8akcani59ivnqzyyv1mzw0jmmc3pfklq7x9b17cm";
      exon = hackage "1.2.0.0" "0il7167fk6bk2ahza2cpzhdjkyvdzwcwfdqcqaxhsv7nj6hckg5l";
      fcf-containers = notest (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      hasql-dynamic-statements = hackage "0.3.1.1" "0pq67kknygp9qjhz5afwmbllf8391czb0m6x9ivla4ddq0cp8plc";
      hasql-implicits = jailbreak unbreak;
      polysemy = hackage "1.7.1.0" "0qwli1kx3hk68hqsgw65mk81bx0djw1wlk17v8ggym7mf3lailyc";
      polysemy-plugin = hackage "0.4.3.0" "1r7j1ffsd6z2q2fgpg78brl2gb0dg8r5ywfiwdrsjd2fxkinjcg1";
    };

    vm = {
      name = "polysemy-db";
      port = 10000;
      postgres = {
        enable = true;
        name = "polysemy-db";
        log = true;
        creds = {
          user = "polysemy-db";
          password = "polysemy-db";
        };
      };
    };

    env = {
      polysemy_db_test_host = "localhost";
      polysemy_db_test_port = vm.port;
    };

  in hix.lib.pro ({ config, lib, ... }: {
    main = "polysemy-hasql";
    overrides = { inherit all; };
    depsFull = [prelate];
    packages = {
      polysemy-db = ./packages/db;
      polysemy-db-data = ./packages/data;
      polysemy-hasql = ./packages/hasql;
    };
    devGhc.compiler = "ghc902";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      preludePackage = "prelate";
      preludeModule = "Prelate";
      extensions = ["StandaloneKindSignatures"];
    };
    hpack.packages = import ./ops/hpack.nix { inherit config lib; };
    ghcid = {
      shellConfig = { inherit vm; };
      testConfig = conf: { inherit env; vm.enable = lib.mkForce (conf.type == "integration"); };
    };
    compat.enable = false;
  });
}
