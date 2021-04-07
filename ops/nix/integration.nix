{ nixpkgs, pkgs, ... }:
let
  port = 10000;
  name = "polysemy-db-test";
  tmp = "/tmp/polysemy-hasql-integration";
  pidfile = "${tmp}/vm.pid";
  image = "${tmp}/vm.qcow2";
  nixos = import "${nixpkgs}/nixos" {
    system = "x86_64-linux";
    configuration = { pkgs, ... }: {
      virtualisation = {
        diskImage = image;
        diskSize = 4096;
        qemu.networkingOptions = [
          "-net nic,netdev=user.0,model=virtio"
          "-netdev user,id=user.0,hostfwd=tcp::${toString port}-:${toString port},hostfwd=tcp::10022-:22"
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
in {
  inherit postgresVm ensurePostgresVm;
}
