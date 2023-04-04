
vim.cmd([[

call plug#begin()
Plug 'w0ng/vim-hybrid'
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'MunifTanjim/nui.nvim'
Plug 'dpayne/CodeGPT.nvim'
Plug 'alexghergh/nvim-tmux-navigation'

call plug#end()


set relativenumber
colorscheme hybrid

syntax sync minlines=200

syntax on
filetype plugin indent on

highlight LineNr ctermfg=DarkGrey
hi clear CursorLine
hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red
hi LineNr guibg=#1D1F21

lua require('nvim-tmux-navigation')

nnoremap <silent> <ESC>h <Cmd>NvimTmuxNavigateLeft<CR>
nnoremap <silent> <ESC>j <Cmd>NvimTmuxNavigateDown<CR>
nnoremap <silent> <ESC>k <Cmd>NvimTmuxNavigateUp<CR>
nnoremap <silent> <ESC>l <Cmd>NvimTmuxNavigateRight<CR>
nnoremap <silent> <ESC>Space <Cmd>NvimTmuxNavigateNext<CR>
]])
-- require("codegpt.config")
--
-- nnoremap <silent> <ESC>h :TmuxNavigateLeft<cr>
-- nnoremap <silent> <ESC>l :TmuxNavigateRight<cr>
-- nnoremap <silent> <ESC>k :TmuxNavigateDown<cr>
-- nnoremap <silent> <ESC>j :TmuxNavigateUp<cr>
