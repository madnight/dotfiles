" vi: ft=vim
"              _                              __ _
"       __   _(_)_ __ ___     ___ ___  _ __  / _(_) __ _
"       \ \ / / | '_ ` _ \   / __/ _ \| '_ \| |_| |/ _` |
"        \ V /| | | | | | | | (_| (_) | | | |  _| | (_| |
"         \_/ |_|_| |_| |_|  \___\___/|_| |_|_| |_|\__, |
"                                                  |___/
filetype plugin indent on
syntax on
let mapleader="\,"

if &compatible
  set nocompatible
endif

"##########################
" Load plugins and configs
"##########################
source ~/vim/functions.vim
source ~/vim/plugins.vim
source ~/vim/gvim.vim
source ~/vim/keys.vim
source ~/vim/plugins-config.vim
source ~/vim/autocmd.vim

function! s:Highlight_Matching_Pair()
endfunction

function! s:FindMatchingPair()
endfunction

function! s:Find_Matching_Pair()
endfunction

"###################
" Basic vim settings
"###################
set background=dark
set backspace=indent,eol,start
set bs=indent,eol,start
set cc=80
set clipboard=unnamedplus
set cmdheight=1
set complete=.,w,b,u,t
set confirm
set cursorline
set diffopt+=context:99999
set encoding=utf-8
set esckeys
set expandtab
set fileformat=unix
set formatoptions=l
set grepprg=rg\ --color\ never\ --line-number\ --no-heading
set hidden
set history=50
set ignorecase
set invnumber
set laststatus=2
set lbr
set list listchars=tab:»-,trail:·,extends:»,precedes:«
set magic
set matchpairs+=<:>
set matchtime=2
set mouse=a
set mouse=v
set nobackup
set nocompatible
set nocursorcolumn
set nocursorline
set nofoldenable
set nolazyredraw
set noruler
set noshowcmd
set nostartofline
set noswapfile
set notimeout ttimeout ttimeoutlen=200
set nowrap
set number
set nuw=1
set pastetoggle=<F11>
set relativenumber
set ruler
set scrolloff=2
set shell=/bin/bash
set shiftwidth=4
set showcmd
set showmatch
set showmode
set smartcase
set smartindent
set softtabstop=4
set t_Co=256
set t_vb=
set tabstop=4
set term=xterm-256color
set timeoutlen=1000 ttimeoutlen=0
set title
set updatetime=1000
set viminfo='20,\"500
set wildignore+=*.o,*.obj,*.bak,*.exe,*.py[co],*.swp,*~,*.pyc,.svn
set wildignore+=*/.git/*,*/tmp/*,*.swp,*.so,*.zip,*/node_modules
set wildmenu
set wildmode=longest,list,full
set wrap

hi OverLength ctermbg=black ctermfg=red
match OverLength /\%81v.\+/

"############
" Typo Fixes
"############
command! WQ wq
command! Wqa wqa
command! W w
command! Q q
command! WS w !sudo tee %

"############
" Colortheme
"############
colorscheme hybrid

syntax sync minlines=200

" move syntax enable to the bottom of vimrc to avoid syntax detection issues
syntax on
filetype plugin indent on

" should be the last lines
highlight LineNr ctermfg=DarkGrey
hi clear CursorLine
hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red
hi LineNr guibg=#1D1F21

function! g:FuckThatMatchParen ()
    if exists(":NoMatchParen")
        :NoMatchParen
    endif
endfunction

augroup plugin_initialize
    autocmd!
    autocmd VimEnter * call FuckThatMatchParen()
augroup END
