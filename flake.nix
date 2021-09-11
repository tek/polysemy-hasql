{
  description = "Polysemy Effects for Databases";


  inputs = {
    chronos = { url = github:andrewthad/chronos/aa6d2b0969c4c5216ff9e45da1574e194fafefc1; flake = false; };
    hix.url = github:tek/hix;
  };

  outputs = { chronos, hix, ... }:
  let
    compat901 = { hackage, source, jailbreak, minimal, noHpack, ... }: {
      cryptohash-md5 = jailbreak (hackage "0.11.100.1" "1l9l3c5x4759pa0ah48skzrkakb5738n6cw60ksj8pmzf68f428a");
      cryptohash-sha1 = jailbreak (hackage "0.11.100.1" "0k3q9sraq7s5y8i01p5a2b3cvbdvslz9kv494vh83jrvsamj7dcx");
      path = hackage "0.9.0" "14symzl1rszvk5zivv85k79anz7xyl5gaxy0sm4vhhzsgxc59msv";
      path-io = jailbreak (hackage "1.6.3" "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc");
      rebase = hackage "1.13.0.1" "19c0yq7392aj2asd826gy0ps4brgmvp7bxa8kxhv8p09sljjgw66";
      rerebase = hackage "1.13.0.1" "0dz0kgi0w1f3a2prw69k43mk01ywh3kc27xskkcpl8bhqmc78n23";
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      th-test-utils = jailbreak (hackage "1.1.0" "1nmpa6hz2zv12drb9w82rwq3f2agn7lw4g3mvj3mrsb3g33g251k");
      type-errors-pretty = jailbreak;
      typerep-map = jailbreak (hackage "0.3.3.0" "15i0h2xczf4x898vjd4vgbb8n10gbsbvy2s2pfw4b3vzf0a1rayl");
    };

    compat = { hackage, jailbreak, source, ... }: {
      exon = hackage "0.1.0.0" "0lwq53zcw6v030yk0v7p6s5cv1gqag2jb56lh3p7xc5qdn06bc6b";
      fcf-containers = jailbreak (hackage "0.6.0" "0wxc5213dcxkmd2j1vkhjqsqsxipv8hbq3jnc0ll4xzrlpqic3wf");
      hasql-dynamic-statements = hackage "0.3.1" "1zjv91xlfkyxwq6mhzj7rsfm4kjvs9ygkgbl6jbbg19jihcn2kiy";
      polysemy-conc = hackage "0.2.0.0" "17w29sn3rqh78ik1jzwg7kvzlxdahy60s7l0ign4p5bhywhdnj6a";
      polysemy-log = hackage "0.2.2.3" "1r5iryp70y2r3w6s7p33nwics6640612jzkz18kzz7jn6lp4xwp5";
      polysemy-resume = hackage "0.1.0.4" "0qkq2vpm6vddk86cm0y2f704cq8hwl6h9iqr6znbddjiz67qf9vj";
      polysemy-test = hackage "0.3.1.7" "0j33f5zh6gyhl86w8kqh6nm02915b4n32xikxc4hwcy7p5l7cl34";
      polysemy-time = hackage "0.1.4.0" "0hwx89cilmsdjs3gb5w6by87ysy24scgj5zg77vbfnqpzr3ifrwh";
    };

    common = { hackage, ... }: {
      polysemy = hackage "1.6.0.0" "15k51ysrfcbkww1562g8zvrlzymlk2rxhcsz9ipsb0q6h571qgvf";
      polysemy-plugin = hackage "0.4.0.0" "0pah1a8h8ckbv2fq20hrikrd1p5a3bdxr03npkyixc6mv5k1rmck";
    };

    main = { hackage, source, minimal, jailbreak, ... }: {
      chronos = minimal (source.root chronos);
      path = hackage "0.9.0" "14symzl1rszvk5zivv85k79anz7xyl5gaxy0sm4vhhzsgxc59msv";
      path-io = jailbreak (hackage "1.6.3" "05hcxgyf6kkz36mazd0fqwb6mjy2049gx3vh8qq9h93gfjkpp2vc");
      relude = hackage "1.0.0.1" "164p21334c3pyfzs839cv90438naxq9pmpyvy87113mwy51gm6xn";
      tasty-hedgehog = hackage "1.1.0.0" "0cs96s7z5csrlwj334v8zl459j5s4ws6gmjh59cv01wwvvrrjwd9";
    };

    preStartCommand = project: ''
      ${(import ./ops/nix/integration.nix project).ensurePostgresVm}
      export polysemy_db_test_host=localhost
      export polysemy_db_test_port=10000
    '';
  in hix.flake {
    base = ./.;
    main = "polysemy-hasql";
    overrides = [compat common main];
    compat = false;
    compatOverrides = { all = compat; ghc901 = [common compat901]; };
    packages = {
      polysemy-db = ./packages/db;
      polysemy-db-data = ./packages/data;
      polysemy-hasql = ./packages/hasql;
    };
    ghci.extraArgs = ["-fplugin=Polysemy.Plugin" "-fprint-potential-instances" "-fconstraint-solver-iterations=20"];
    versionFile = "ops/hpack/shared/meta.yaml";
    runConfig = project: { preStartCommand = preStartCommand project; };
    ghcid.easy-hls = false;
  };
}
