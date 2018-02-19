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
" use vim-defaults instead of vi-defaults
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
source ~/.private_vimrc
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
set shell=/bin/bash
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
" case insensitive searching
set ignorecase
" but become case sensitive if you type uppercase characters
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
" set re=1
" lazy redraw + tmux => rendering glitches
set nolazyredraw
set nocursorcolumn
set nocursorline
set background=dark
set nuw=1
set updatetime=1000
set wildignore+=*/.git/*,*/tmp/*,*.swp,*.so,*.zip,*/node_modules
" dont wrap lines
set nowrap
" 2 lines above/below cursor when scrolling
set scrolloff=2
" show line numbers
set number
" show matching bracket (briefly jump)
set showmatch
" show mode in status bar (insert/replace/...)
set showmode
" show typed command in status bar
set showcmd
" show cursor position in status bar
set ruler
" show file in titlebar
set title
" completion with menu
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe,*.py[co],*.swp,*~,*.pyc,.svn
" use 2 lines for the status bar
set laststatus=2
" show matching bracket for 0.2 seconds
set matchtime=2
" specially for html
set matchpairs+=<:>
" map missed escape sequences (enables keypad keys)
set esckeys
" case insensitive searching
set ignorecase
" but become case sensitive if you type uppercase characters
" set smartcase
" smart auto indenting
" set smartindent
" smart tab handling for indenting
" set smarttab
" change the way backslashes are used in search patterns
set magic
" Allow backspacing over everything in insert mode
set bs=indent,eol,start
" number of spaces a tab counts for
set tabstop=4
" spaces for autoindents
set shiftwidth=4
" file mode is unix
set fileformat=unix
" get a dialog when :q, :w, or :wq fails
set confirm
" no backup~ files.
set nobackup
" remember copy registers after quitting in the .viminfo file -- 20 jump links, regs up to 500 lines'
set viminfo='20,\"500
" remember undo after quitting
set hidden
" keep 50 lines of command history
set history=50
" use mouse in visual mode (not normal,insert,command,help mode
set mouse=v
" soft wrap
set wrap
set relativenumber
set cc=80
set complete=.,w,b,u,t

" https://stackoverflow.com/questions/235439/vim-80-column-layout-concerns
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














