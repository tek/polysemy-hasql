{
  description = "Polysemy Effects for Databases";

  inputs = {
    polysemy-log.url = github:tek/polysemy-log;
    polysemy-resume.url = github:tek/polysemy-resume;
  };

  outputs = { polysemy-log, polysemy-resume, ... }:
  let
    inherit (polysemy-log.inputs) polysemy-conc;
    inherit (polysemy-conc.inputs) polysemy-time;
    inherit (polysemy-time.inputs) polysemy-test chronos;
    inherit (polysemy-test.inputs) tryp-hs;

    common = { hackage, source, jailbreak, ... }: {
      fcf-containers = jailbreak (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      polysemy = hackage "1.5.0.0" "1xl472xqdxnp4ysyqnackpfn6wbx03rlgwmy9907bklrh557il6d";
      polysemy-plugin = hackage "0.3.0.0" "1frz0iksmg8bpm7ybnpz9h75hp6hajd20vpdvmi04aspklmr6hj0";
      co-log = jailbreak (hackage "0.4.0.1" "05f37lq1kwlmm62n1n932l8jnqirmlf87var2n2zb0cslmv63yxg");
      co-log-polysemy = jailbreak (hackage "0.0.1.2" "17bcs8dvrhwfcyklknkqg11gxgxm2jaa7kbm6xx4vm1976abzwss");
    };

    main = { hackage, source, jailbreak, ... }: {
      path = hackage "0.8.0" "0isldidz2gypw2pz399g6rn77x9mppd1mvj5h6ify4pj4mpla0pb";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      polysemy-test = source.sub polysemy-test "packages/polysemy-test";
      polysemy-time = source.sub polysemy-time "packages/time";
      polysemy-conc = source.sub polysemy-conc "packages/conc";
      polysemy-log = source.sub polysemy-log "packages/polysemy-log";
      polysemy-resume = source.sub polysemy-resume "packages/resume";
    };

    compat = { hackage, source, versions, jailbreak, ... }: {
      polysemy-test = hackage "0.3.1.1" "0x0zg1kljr7a1mwmm3zrmha5inz3l2pkldnq65fvsig8f3x8rsar";
      polysemy-time = hackage "0.1.2.1" "09l8r5fx0vnapdn8p0cwiwprgg3i67m58dd4j5hcdhw34gfqnnsr";
      polysemy-conc = hackage "0.1.0.2" "0ijz5l8q53d1s7i100gvjdhzv80dpd140m7a9hyam113ybglc8lg";
      polysemy-log = hackage "0.2.2.1" "1c8kn28a5j9k52jfg5n6nb4ywd76mgrgaqwmf1q0km2kgdi9y40s";
      polysemy-resume = hackage "0.1.0.2" "08sm3db7vpwj3xhlqn98yfpn1hlzv00jkah9q02k1ym024g5iir0";
    };

    preStartCommand = project: ''
      ${(import ./ops/nix/integration.nix project).ensurePostgresVm}
      export polysemy_db_test_host=localhost
      export polysemy_db_test_port=10000
    '';
  in
  tryp-hs.flake {
    base = ./.;
    main = "polysemy-hasql";
    overrides = tryp-hs.composeCabal [common main];
    compatOverrides = tryp-hs.composeCabal [common compat];
    packages = {
      polysemy-db = "packages/db";
      polysemy-db-data = "packages/data";
      polysemy-hasql = "packages/hasql";
    };
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin"];
    versionFile = "ops/hpack/shared/meta.yaml";
    runConfig = project: { preStartCommand = preStartCommand project; };
  };
}
