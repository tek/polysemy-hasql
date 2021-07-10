{
  description = "Polysemy Effects for Databases";


  inputs = {
    hix.url = github:tek/hix;
    polysemy.url = github:polysemy-research/polysemy;
    chronos = { url = github:andrewthad/chronos/aa6d2b0969c4c5216ff9e45da1574e194fafefc1; flake = false; };
  };

  outputs = { hix, polysemy, chronos, ... }:
  let
    compat901 = { hackage, source, jailbreak, minimal, noHpack, ... }: {
      cryptohash-md5 = jailbreak (hackage "0.11.100.1" "1l9l3c5x4759pa0ah48skzrkakb5738n6cw60ksj8pmzf68f428a");
      cryptohash-sha1 = jailbreak (hackage "0.11.100.1" "0k3q9sraq7s5y8i01p5a2b3cvbdvslz9kv494vh83jrvsamj7dcx");
      path = hackage "0.9.0" "14symzl1rszvk5zivv85k79anz7xyl5gaxy0sm4vhhzsgxc59msv";
      path-io = jailbreak (hackage "1.6.3" "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc");
      polysemy = noHpack (minimal (source.root polysemy));
      polysemy-plugin = noHpack (minimal (source.sub polysemy "polysemy-plugin"));
      rebase = hackage "1.13.0.1" "19c0yq7392aj2asd826gy0ps4brgmvp7bxa8kxhv8p09sljjgw66";
      rerebase = hackage "1.13.0.1" "0dz0kgi0w1f3a2prw69k43mk01ywh3kc27xskkcpl8bhqmc78n23";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      th-test-utils = jailbreak (hackage "1.1.0" "1nmpa6hz2zv12drb9w82rwq3f2agn7lw4g3mvj3mrsb3g33g251k");
      type-errors-pretty = jailbreak;
      typerep-map = jailbreak (hackage "0.3.3.0" "15i0h2xczf4x898vjd4vgbb8n10gbsbvy2s2pfw4b3vzf0a1rayl");
    };

    common = { hackage, jailbreak, ... }: {
      fcf-containers = jailbreak (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      hasql-dynamic-statements = hackage "0.3.1" "1zjv91xlfkyxwq6mhzj7rsfm4kjvs9ygkgbl6jbbg19jihcn2kiy";
      polysemy-conc = hackage "0.1.0.2" "0ijz5l8q53d1s7i100gvjdhzv80dpd140m7a9hyam113ybglc8lg";
      polysemy-log = hackage "0.2.2.1" "1c8kn28a5j9k52jfg5n6nb4ywd76mgrgaqwmf1q0km2kgdi9y40s";
      polysemy-resume = hackage "0.1.0.3" "0wz3wrib5gj1bgzlis2ixx753nfdw4y601blc82356k4mi1lp138";
      polysemy-test = hackage "0.3.1.5" "0b8czkn1z27053zw176j6hrpdaw8g31g701v5fp0005wqlngn08x";
      polysemy-time = hackage "0.1.3.0" "1wdnq49g7g09q0bdp18h90iirzlm65dq77ljnwgagm6imrr7886l";
    };

    main = { hackage, source, minimal, jailbreak, ... }: {
      chronos = minimal (source.root chronos);
      path = hackage "0.9.0" "14symzl1rszvk5zivv85k79anz7xyl5gaxy0sm4vhhzsgxc59msv";
      path-io = jailbreak (hackage "1.6.3" "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc");
      polysemy = source.root polysemy;
      polysemy-plugin = source.sub polysemy "polysemy-plugin";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      run-st = hackage "0.1.1.0" "0w1wrbzc7gkgzlyigz3hvxbpb8h2h6bl6rmyrmydxgys9ywn5cwm";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
      wide-word = hackage "0.1.1.2" "16czh619fiwximz3sxk32f2776saqvr0yrz6kv3bddwxhwxj09rf";
    };

    preStartCommand = project: ''
      ${(import ./ops/nix/integration.nix project).ensurePostgresVm}
      export polysemy_db_test_host=localhost
      export polysemy_db_test_port=10000
    '';
  in hix.flake {
    base = ./.;
    main = "polysemy-hasql";
    overrides = [common main];
    # overrides = [common compat901];
    # compiler = "ghc901";
    # ghcid.easy-hls = false;
    compat = false;
    compatOverrides = { all = common; ghc901 = compat901; };
    packages = {
      polysemy-db = ./packages/db;
      polysemy-db-data = ./packages/data;
      polysemy-hasql = ./packages/hasql;
    };
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin" "-fprint-potential-instances" "-fconstraint-solver-iterations=20"];
    versionFile = "ops/hpack/shared/meta.yaml";
    runConfig = project: { preStartCommand = preStartCommand project; };
  };
}
