{ config, pkgs, ... }:

{programs.neovim = {
    enable = true;
    defaultEditor = true;
    #viAlias = true;
    #vimAlias = true;
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
}
