{ config, pkgs, ... }:

{
  programs.tmux = {
  enable = true;
  clock24 = true;
  extraConfig = ''
    set-option -sa terminal-overrides ",xterm*:Tc"
    set-option -g renumber-windows on
    unbind C-b
    set -g prefix C-x
    bind C-x send-prefix
    set -g mode-keys emacs
    set -g status-keys emacs
    set -s escape-time 0
    set -g mouse on
    '';
  };

}
