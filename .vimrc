"                 |                                                        _|_|  _|
"     _|      _|      _|_|_|  _|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|
"     _|      _|  _|  _|    _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|
"       _|  _|    _|  _|    _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|
"         _|      _|  _|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|
"                                                                                           _|
"                                                                                       _|_|

filetype plugin indent on
syntax on
let mapleader="\,"

" Load plugins, functions and gui settings
source ~/vim/plugins.vim
source ~/vim/functions.vim
source ~/vim/gvim.vim
source ~/vim/keys.vim
source ~/vim/plugins-config.vim

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

" Jump to the last known cursor position when opening a file.
augroup vimrc
  au!
  au BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
augroup END

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
set invnumber
set t_Co=256
set term=xterm-256color
set noswapfile
set nocompatible
set laststatus=2
set cmdheight=1
set noruler
set noshowcmd

set background=dark

hi clear CursorLine
augroup CLClear
  au! ColorScheme * hi clear CursorLine
augroup END

colorscheme hybrid

au BufNewFile,BufRead,BufEnter *.tex setlocal spell spelllang=de_de,en_gb
au BufNewFile,BufRead,BufEnter *.txt setlocal spell spelllang=de_de,en_gb
au BufNewFile,BufRead,BufEnter *.pl set filetype=prolog
au BufRead,BufNewFile,BufEnter *mutt* set filetype=mail
au BufNewFile,BufRead *.coffee set filetype=coffee

au FileType php set omnifunc=phpcomplete#CompletePHP
au FileType mail setlocal spell spelllang=de_de,en_gb
au FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
au FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
au FileType ruby,eruby let g:rubycomplete_rails = 1
au FileType ruby set omnifunc=rubycomplete#Complete
au FileType css setlocal omnifunc=csscomplete#CompleteCSS
au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
au FileType python setlocal omnifunc=pythoncomplete#Complete
au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
au CursorHold * call ale#Lint()
au CursorHoldI * call ale#Lint()
au InsertLeave * call ale#Lint()
au TextChanged * call ale#Lint()

hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

highlight LineNr guibg=#1D1F21
set nuw=1

" Linting on all changes felt too aggressive.
set updatetime=1000

" vim:ft=vim
" highlight OverLength ctermbg=red ctermfg=white guibg=#592929
" match OverLength /\%81v.\+/
" highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

source ~/.private_vimrc

let g:tmux_navigator_no_mappings = 1

nnoremap <silent> {Left-Mapping} :TmuxNavigateLeft<cr>
nnoremap <silent> {Down-Mapping} :TmuxNavigateDown<cr>
nnoremap <silent> {Up-Mapping} :TmuxNavigateUp<cr>
nnoremap <silent> {Right-Mapping} :TmuxNavigateRight<cr>
nnoremap <silent> {Previous-Mapping} :TmuxNavigatePrevious<cr>
