{ config, pkgs, ... }:

{
    programs = {
        zsh = {
            enable = true;
            oh-my-zsh = {
                enable = true;
                theme = "clean";
                plugins = [
                    "git" "man" "thefuck" "copybuffer" "copypath"
                ];
            };

            enableAutosuggestions = true;
            enableCompletion = true;
            enableSyntaxHighlighting = true;
        };
    };
}
