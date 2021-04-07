{
  description = "Polysemy Effects for Databases";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/c0e881852006b132236cbf0301bd1939bb50867e;
    tryp-hs = {
      # url = github:tek/tryp-hs;
      url = path:/home/tek/code/tek/nix/tryp-hs;
      inputs.nixpkgs.follows = "nixpkgs";
    };
    polysemy-test.url = github:tek/polysemy-test;
    polysemy-time.url = github:tek/polysemy-time;
    polysemy-conc.url = github:tek/polysemy-conc;
    polysemy-log.url = github:tek/polysemy-log;
    polysemy-resume.url = github:tek/polysemy-resume;
  };

  outputs = { tryp-hs, polysemy-test, polysemy-time, polysemy-conc, polysemy-log, polysemy-resume, ... }:
  let
    common = { hackage, source, jailbreak, ... }: {
      fcf-containers = jailbreak (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
      polysemy-plugin = hackage "0.3.0.0" "1frz0iksmg8bpm7ybnpz9h75hp6hajd20vpdvmi04aspklmr6hj0";
      co-log = jailbreak (hackage "0.4.0.1" "05f37lq1kwlmm62n1n932l8jnqirmlf87var2n2zb0cslmv63yxg");
      co-log-polysemy = jailbreak (hackage "0.0.1.2" "17bcs8dvrhwfcyklknkqg11gxgxm2jaa7kbm6xx4vm1976abzwss");
    };

    overrides = { hackage, source, jailbreak, ... }@args: common args // {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      polysemy-test = source.sub polysemy-test "packages/polysemy-test";
      polysemy-time = source.sub polysemy-time "packages/time";
      polysemy-conc = source.sub polysemy-conc "packages/conc";
      polysemy-log = source.sub polysemy-log "packages/polysemy-log";
      polysemy-resume = source.sub polysemy-resume "packages/resume";
    };

    compatOverrides = { hackage, source, versions, jailbreak, ... }@args: common args // {
      polysemy-test = hackage "0.3.1.1" "0x0zg1kljr7a1mwmm3zrmha5inz3l2pkldnq65fvsig8f3x8rsar";
      polysemy-time = hackage "0.1.2.1" "09l8r5fx0vnapdn8p0cwiwprgg3i67m58dd4j5hcdhw34gfqnnsr";
      polysemy-conc = hackage "0.1.0.2" "0ijz5l8q53d1s7i100gvjdhzv80dpd140m7a9hyam113ybglc8lg";
      polysemy-log = hackage "0.2.2.1" "1c8kn28a5j9k52jfg5n6nb4ywd76mgrgaqwmf1q0km2kgdi9y40s";
      polysemy-resume = hackage "0.1.0.2" "08sm3db7vpwj3xhlqn98yfpn1hlzv00jkah9q02k1ym024g5iir0";
    };
  in
  tryp-hs.flake {
    base = ./.;
    compiler = "ghc8104";
    main = "polysemy-hasql";
    overrides = tryp-hs.overrides overrides;
    compatOverrides = tryp-hs.overrides compatOverrides;
    packages = {
      polysemy-db = "packages/db";
      polysemy-db-data = "packages/data";
      polysemy-hasql = "packages/hasql";
    };
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    versionFile = "ops/hpack/shared/meta.yaml";
    modify = project: outputs:
    let
      vm = (import ./ops/nix/integration.nix project).ensurePostgresVm;
      preCmd = ''
        ${vm}
        export polysemy_db_test_host=localhost
        export polysemy_db_test_port=10000
      '';
    in {
      legacyPackages.run = outputs.legacyPackages.run.override { preStartCommand = preCmd; };
    };
  };
}
