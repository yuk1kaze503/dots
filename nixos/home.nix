{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sn0w";
  home.homeDirectory = "/home/sn0w";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  
  home.packages = with pkgs; [
    okular
    p7zip
    gnomeExtensions.hide-top-bar-window
    stdman
    emacsPackages.vterm
    thunderbird
    tree
    nodePackages.typescript-language-server
  ];

  programs.neovim = {
    enable = true;
    extraConfig = '' 
      set background=dark
      colorscheme everforest
      set mouse=a
      set number
      set termguicolors
      set encoding=utf-8
      set updatetime=300
      set signcolumn=yes
      set autoindent
      set smartindent
      set breakindent
      set hlsearch
      set cmdheight=2
      set laststatus=2
      set expandtab
      set smarttab
      set shiftwidth=2
      set tabstop=2
      imap jj <ESC>
      function! CheckBackspace() abort
        let col = col('.') - 1
        return !col || getline('.')[col - 1]  =~# '\s'
      endfunction
      inoremap <silent><expr> <TAB>
      \ coc#pum#visible() ? coc#pum#next(1) :
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()           
    '';
    plugins = with pkgs.vimPlugins; [
      context-vim
      editorconfig-vim
      gruvbox-community
      #dracula-vim
      everforest
      vim-airline
      vim-nix
      fern-vim
      coc-nvim
      coc-clangd
      coc-cmake
      coc-go
      coc-json
      coc-java
      coc-lua
      coc-pairs
      coc-python
      coc-tsserver
      coc-rust-analyzer
      nvim-treesitter
      #rust-vim
      #vim-go
      #typescript-vim
      neogit
    ];
  };
  
  programs.git = {
    enable = true;
    userName = "FRX-00";
    userEmail = "nigetx1016@gmail.com";
    #userPassword = "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";
  };
}
