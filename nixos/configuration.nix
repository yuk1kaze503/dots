# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Bootloader.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efi";
    grub = {
      enable = true;
      devices = [ "nodev" ];
      efiSupport = true;
      useOSProber = true;
    };
  };
 
  time.hardwareClockInLocalTime = true;
  
  
  networking.hostName = "n1x0s"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Tokyo";

  # VM
  virtualisation.docker.enable = true;
  
  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = ["en_US.UTF-8/UTF-8" "zh_CN.UTF-8/UTF-8" "ja_JP.UTF-8/UTF-8" ];
  };

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  i18n.inputMethod = { 
    #enabled = "fcitx5";
    #fcitx5.addons = with pkgs;
    #[ fcitx5-mozc fcitx5-hangul fcitx5-rime fcitx5-chinese-addons ];
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines;
    [ mozc hangul rime];
  };

  # fonts
  fonts.fonts = with pkgs; [
  unicode-emoji
  emojione
  nerdfonts
  source-han-serif
  source-han-mono
  sarasa-gothic
  jetbrains-mono
  cascadia-code
  hack-font
  ];
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the KDE Plasma Desktop Environment.
  #services.xserver.displayManager.sddm.enable = true;
  #services.xserver.desktopManager.plasma5.enable = true;
  #
  # Gnome
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sn0w = {
    isNormalUser = true;
    description = "sn0w";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      kate
    #  thunderbird
    ];
  };
  # ZSH
  programs.zsh.enable = true;
  programs.zsh.syntaxHighlighting.enable = true;
  programs.zsh.autosuggestions.enable = true;
  programs.zsh.ohMyZsh = {
    enable = true;
    plugins = [ "git" "man" "thefuck"];
    theme = "ys";
  };
  # tmux
  programs.tmux = {
    enable = true;
    clock24 = true;
    extraConfig = ''
      unbind C-b
      set -g prefix C-x
      bind C-x send-prefix
      set -g mode-keys emacs
      set -g status-keys emacs
      set -s escape-time 0
    '';
  };
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Nvidia driver
  services.xserver.videoDrivers = ["nvidia"];
  hardware.opengl.enable = true;
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    emacs
    vscode
    # terminal
    gnome.gnome-terminal
    kitty
    # utilities
    wget
    curl
    git
    zsh
    tmux
    unzip
    unrar
    ranger
    htop
    zsh
    thefuck
    bat
    neofetch
    trash-cli
    #webkitgtk
    #glib
    # web browser
    firefox
    google-chrome
    # dev
    llvmPackages_15.llvm
    clang_15
    clang-tools_15   
    gcc
    gdb
    cmake
    libtool
    libvterm
    python310
    python310Packages.ipython
    python310Packages.pip
    python-language-server
    jdk
    chez
    nodejs
    nodePackages.typescript
    rustup
    ocaml
    go
    # lsp
    rust-analyzer
    gopls
    emacsPackages.eglot-java
    # vm
    docker
    qemu
    virtualbox
    # net-things
    openssl
    openvpn
    vagrant
    # etc
    jetbrains.idea-community
    obsidian
    pinta
    ffmpeg
    mpv
    spotify
    # themes
    gnome.gnome-tweaks
    flat-remix-gnome
    papirus-icon-theme
    ];
  # gc
  nix.gc = {
    automatic = true;
    dates = "monthly";
    options = "--delete-older-than 30d";
  };  

  # system environment
  services.accounts-daemon.enable = true;
  services.gnome.gnome-online-accounts.enable = true;
  #environment.variables = {
  #  WEBKIT_FORCE_SANDBOX = "0";
  #};

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # Automatic Upgrades
  #system.autoUpgrade.enable = true;
  #system.autoUpgrade.allowReboot = true;
  
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
  
}
