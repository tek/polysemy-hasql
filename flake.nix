{
  description = "Polysemy effects for databases";

  inputs = {
    hix.url = git+https://git.tryp.io/tek/hix;
    prelate.url = git+https://git.tryp.io/tek/prelate;
    hls.url = github:haskell/haskell-language-server;
  };

  outputs = { hix, prelate, hls, ... }:
  let

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
    depsFull = [prelate];
    packages = {
      sqel = ./packages/sqel;
      polysemy-db = ./packages/db;
      polysemy-hasql = ./packages/hasql;
      polysemy-hasql-test = ./packages/hasql-test;
    };
    devGhc.compiler = "ghc925";
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
    shell.hls.package = hls.packages.${config.system}.haskell-language-server-925;
  });
}
