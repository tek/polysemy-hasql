{ nixpkgs, pkgs, ... }:
{ dbName ? "polysemy-db-test", port ? 10000, log ? false }:
let
  name = "polysemy-db";

  tmp = "/tmp/polysemy-hasql-integration/${dbName}";

  pidfile = "${tmp}/vm.pid";

  image = "${tmp}/vm.qcow2";

  nixos = import "${nixpkgs}/nixos" {
    system = "x86_64-linux";
    configuration = { pkgs, ... }: {
      virtualisation = {
        diskImage = image;
        diskSize = 4096;
        forwardPorts = [
          { from = "host"; host.port = port + 22; guest.port = 22; }
          { from = "host"; host.port = port; guest.port = port; }
        ];
      };
      services.openssh = {
        enable = true;
        permitRootLogin = "yes";
      };
      users.mutableUsers = false;
      users.users.root.password = "";
      networking.firewall.enable = false;
      services.postgresql = {
        enable = true;
        inherit port;
        package = pkgs.postgresql_13;
        enableTCPIP = true;
        ensureDatabases = [name];
        authentication = ''
          host all all 0.0.0.0/0 md5
          host all all ::/0 md5
        '';
        initialScript = pkgs.writeText "polysemy-db-postgresql-init" ''
          create role "${name}" with login password '${name}' createdb;
          grant all privileges on database "${name}" to "${name}";
        '';
        settings = {
          log_statement = "all";
          log_min_messages = "info";
        };
      };
    };
  };

  postgresVm = nixos.vm;

  ensurePostgresVm = pkgs.writeScript "ensure-postgres-vm" ''
    #!${pkgs.zsh}/bin/zsh
    if ${pkgs.procps}/bin/pgrep -F ${pidfile} -L -f ${pidfile} &>/dev/null
    then
      print '>>> vm already running' >&2
    else
      print '>>> starting vm' >&2
      mkdir -p ${tmp}
      rm -f ${pidfile}
      ${postgresVm}/bin/run-nixos-vm -display none -daemonize -pidfile ${pidfile}
    fi
  '';

  killPostgresVm = pkgs.writeScript "kill-postgres-vm" ''
    #!${pkgs.zsh}/bin/zsh
    pid=$(${pkgs.procps}/bin/pgrep -F ${pidfile} -L -f ${pidfile})
    if [[ $? == 0 ]]
    then
      print '>>> killing vm' >&2
      kill $pid
    else
      print '>>> vm not running' >&2
    fi
  '';

  integration = ''
    ${ensurePostgresVm}
    export polysemy_db_test_host=localhost
    export polysemy_db_test_port=${toString port}
  '';

in {
  inherit postgresVm ensurePostgresVm killPostgresVm integration;
}
