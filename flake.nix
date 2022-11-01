{
  description = "Polysemy Effects for Databases";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
  };

  outputs = { hix, prelate, ... }:
  let

    all = { hackage, jailbreak, source, notest, unbreak, ... }: {
      fcf-containers = notest (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      hasql-dynamic-statements = hackage "0.3.1.1" "0pq67kknygp9qjhz5afwmbllf8391czb0m6x9ivla4ddq0cp8plc";
      hasql-implicits = jailbreak unbreak;
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
    overrides = { inherit all; dev = all; };
    depsFull = [prelate];
    packages = {
      sqel = ./packages/sqel;
      polysemy-db = ./packages/db;
      polysemy-hasql = ./packages/hasql;
    };
    devGhc.compiler = "ghc902";
    ghci = {
      args = ["-fplugin=Polysemy.Plugin" "-fprint-potential-instances"];
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
