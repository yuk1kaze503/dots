require("plugins")
require("nvim-tree").setup()
vim.cmd("autocmd!")

vim.scriptencoding = 'utf-8'
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'

vim.opt.backspace = '2'
vim.opt.showcmd = true
vim.opt.laststatus = 2
vim.opt.autowrite = true
-- vim.opt.cursorline = true
vim.opt.autoread = true

-- use spaces for tabs and wahtnot
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.shiftround = true


--
vim.opt.mouse = 'a'
vim.opt.title = true
vim.opt.autoindent = true
vim.opt.smartindent = true
vim.opt.hlsearch = true
vim.opt.backup = false
vim.opt.showcmd = true
vim.opt.cmdheight = 2
vim.opt.laststatus = 2
vim.opt.scrolloff = 10
vim.opt.shell = 'zsh'
vim.opt.updatetime = 300
vim.opt.wrap = true
vim.opt.hidden = true
vim.opt.swapfile = false
vim.opt.number = true
vim.wo.number = true
vim.opt.inccommand = 'split'
vim.opt.ignorecase = true
vim.opt.clipboard = 'unnamedplus'
vim.opt.signcolumn = "yes"
local keymap = vim.keymap

-- colorscheme
vim.opt.termguicolors = true
vim.cmd [[colorscheme everforest]]
-- python3
vim.cmd([[
    let g:python3_host_prog = '/opt/homebrew/opt/python@3.10/libexec/bin/python3'
]])


-- split window
keymap.set('n', 'ss', ':split<Return><C-w>w')
keymap.set('n', 'sv', ':split<Return><C-w>w')

-- move in windows
keymap.set('', 'sh', '<C-w>h')
keymap.set('', 'sk', '<C-w>k')
keymap.set('', 'sj', '<C-w>j')
keymap.set('', 'sl', '<C-w>l')

-- jj as ESC
keymap.set('i', 'jj', '<ESC>')

-- use <tab> to trigger completion and navigate to next complete item.

function _G.check_back_space()
    local col = vim.fn.col('.') - 1
    return col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') ~= nil
end

local opts = {silent = true, noremap = true, expr = true, replace_keycodes = false}
keymap.set("i", "<TAB>", 'coc#pum#visible() ? coc#pum#next(1) : v:lua.check_back_space() ? "<TAB>" : coc#refresh()', opts)
keymap.set("i", "<S-TAB>", [[coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"]], opts)
