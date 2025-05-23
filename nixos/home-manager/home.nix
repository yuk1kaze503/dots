{ config, pkgs, ... }:

let 
  unstable = import <nixpkgs-unstable> {};

in
{
  imports = [
    ./kitty
    ./zsh
    ./nvim
    ./tmux
  ];
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "snow";
  home.homeDirectory = "/home/snow";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.
  
  # allowUnfree
  nixpkgs.config.allowUnfree = true;
  
  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # apps
    unstable.vscodium
    google-chrome
    okular
    steam
    discord
    kitty
    jetbrains.idea-community
    pinta
    mpv
    spotify
    
    # utils
    android-studio
    ranger
    git
    curl
    dunst
    wget
    tty-clock
    tmux
    thefuck
    trash-cli
    unzip
    unrar
    htop
    bat
    yazi
    stdman
    tree
    ffmpeg
    gnome.gnome-tweaks
    emacsPackages.vterm
    emacsPackages.yasnippet-snippets
    # lang
    libvterm
    python313
    python313Packages.ipython
    python313Packages.pip
    jdk21
    kotlin
    chez
    unstable.nodejs
    unstable.nodePackages.npm
    unstable.nodePackages.typescript
    bun  # js runtime
    deno # js runtime
    ocaml
    go
    ruby
    rbenv

    # lsp
    unstable.rust-analyzer
    gopls
    python310Packages.python-lsp-server
    kotlin-language-server
    java-language-server
    unstable.nodePackages.typescript-language-server
    unstable.jdt-language-server
    # vm
    docker
    qemu
    # net-things
    openssl
    openvpn
    vagrant

    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/snow/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
