{ config, pkgs, ... }:

let
  staticDir = builtins.path {
    path = ./my-web-app/static;
    name = "my-web-app-static";
  };
  stateDir = "/var/my-web-app";
  clientSessionKeyName = "client-session";
  myWebAppConfigFile = pkgs.writeText "my-web-app-config" ''
    client-session-key-path: "/run/keys/${clientSessionKeyName}"
    static-dir: "${staticDir}"
    generated-dir: "${stateDir}"
  '';
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [ (import ./overlays/overlay.nix) ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/xvda"; # or "nodev" for efi only

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = ???;
  networking.interfaces.eth0.useDHCP = ???;
  networking.interfaces.eth0.ipv4.addresses = [{
    address = ???;
    prefixLength = ???;
  }];
  networking.defaultGateway = { address = ???; interface = ???; };
  networking.nameservers = [???];
  networking.firewall.allowedTCPPorts = [3000];

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_11;
    authentication = pkgs.lib.mkOverride 10 ''
      local all postgres         ident
      local all all              md5
      host  all all      ::1/128 md5
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE USER "my-web-app" WITH PASSWORD 'my-web-app';
      CREATE DATABASE "my-web-app" WITH OWNER "my-web-app";
    '';
  };

  systemd.services.my-web-app =
  { description = "my-web-app";
    after = [ "${clientSessionKeyName}-key.service" ];
    wants = [ "${clientSessionKeyName}-key.service" ];
    serviceConfig =
      { User = "mywebsrv";
        Group = "mywebsrv";
        ExecStart = "${pkgs.haskellPackages.my-web-app}/bin/my-web-app ${myWebAppConfigFile}";
        WorkingDirectory = stateDir;
      };
  };

  users.users = {
    mywebsrv = {
      description = "mywebsrv";
      home = stateDir;
      createHome = true;
      useDefaultShell = true;
      group = "mywebsrv";
      isSystemUser = true;
    };
  };

  users.groups.mywebsrv = {};

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}

