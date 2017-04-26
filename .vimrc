
"              _                              __ _
"       __   _(_)_ __ ___     ___ ___  _ __  / _(_) __ _
"       \ \ / / | '_ ` _ \   / __/ _ \| '_ \| |_| |/ _` |
"        \ V /| | | | | | | | (_| (_) | | | |  _| | (_| |
"         \_/ |_|_| |_| |_|  \___\___/|_| |_|_| |_|\__, |
"                                                  |___/

filetype plugin indent on
syntax on
let mapleader="\,"

" Load plugins, functions and gui settings
source ~/vim/functions.vim
source ~/vim/plugins.vim
source ~/vim/gvim.vim
source ~/vim/autocmd.vim
source ~/vim/keys.vim
source ~/vim/plugins-config.vim
source ~/.private_vimrc

colorscheme hybrid

silent! iunmap (
silent! iunmap )
silent! iunmap {
silent! iunmap }

" Vim Typo Fixes
command! WQ wq
command! Wq wq
command! Wqa wqa
command! W w
command! Q q
command! WS w !sudo tee %
command! CtrlPFunky call plug#load('ctrlp.vim', 'ctrlp-funky') | CtrlPFunky

set shell=/bin/bash
" The width of a TAB is set to 4.
set tabstop=4
" Indents will have a width of 4
set shiftwidth=4
" Sets the number of columns for a TAB
set softtabstop=4
" Expand TABs to spaces
set expandtab
set smartindent
set list listchars=tab:»-,trail:·,extends:»,precedes:«
set grepprg=rg\ --color\ never\ --line-number\ --no-heading
set wildmode=longest,list,full
set wildmenu
set hidden
set cursorline
set ignorecase
set smartcase
set backspace=indent,eol,start
set nostartofline
set ruler
set clipboard=unnamedplus
set confirm
set encoding=utf-8
set t_vb=
set mouse=a
set notimeout ttimeout ttimeoutlen=200
set pastetoggle=<F11>
set formatoptions=l
set lbr
set nofoldenable
" disable folds for diffs
set diffopt+=context:99999
set invnumber
set t_Co=256
set term=xterm-256color
set noswapfile
set nocompatible
set laststatus=2
set cmdheight=1
set noruler
set noshowcmd
set timeoutlen=1000 ttimeoutlen=0
" old regex engine is much faster
set re=1
set lazyredraw
set nocursorcolumn
set nocursorline
set norelativenumber
syntax sync minlines=200

set background=dark
set nuw=1
set updatetime=1000
set wildignore+=*/.git/*,*/tmp/*,*.swp,*.so,*.zip,*/node_modules

hi clear CursorLine
hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red
hi LineNr guibg=#1D1F21


