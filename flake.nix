{
  description = "Polysemy Effects for Databases";


  inputs = {
    chronos = { url = github:andrewthad/chronos/aa6d2b0969c4c5216ff9e45da1574e194fafefc1; flake = false; };
    hix.url = github:tek/hix;
    incipit.url = github:tek/incipit;
  };

  outputs = { chronos, hix, incipit, ... }:
  let

    all = { hackage, jailbreak, source, notest, ... }: {
      flatparse = hackage "0.3.2.0" "01w71985b9ndg4wkfxqxjj7f1cynji6vp71akr7ivpmxn2drxspa";
      exon = hackage "0.3.0.0" "0jgpj8818nhwmb3271ixid38mx11illlslyi69s4m0ws138v6i18";
      fcf-containers = notest (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      hasql-dynamic-statements = hackage "0.3.1" "1zjv91xlfkyxwq6mhzj7rsfm4kjvs9ygkgbl6jbbg19jihcn2kiy";
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

  in hix.lib.flake ({ lib, ... }: {
    base = ./.;
    main = "polysemy-hasql";
    overrides = { inherit all; };
    depsFull = [incipit];
    packages = {
      polysemy-db = ./packages/db;
      polysemy-db-data = ./packages/data;
      polysemy-hasql = ./packages/hasql;
    };
    ghci = {
      args = ["-fplugin=Polysemy.Plugin"];
      preludePackage = "incipit";
    };
    hackage.versionFile = "ops/hpack/shared/meta.yaml";
    ghcid.shellConfig = { inherit vm; };
    ghcid.testConfig = conf: { inherit env; vm.enable = lib.mkForce (conf.type == "integration"); };
    compat.enable = false;
  });
}
