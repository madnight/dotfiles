"                 _|                                                        _|_|  _|
"     _|      _|      _|_|_|  _|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|
"     _|      _|  _|  _|    _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|
"       _|  _|    _|  _|    _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|
"         _|      _|  _|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|
"                                                                                           _|
"                                                                                       _|_|

"let base16colorspace=256  " Access colors present in 256 colorspace
set smartindent
set rtp+=~/.vim/bundle/Vundle.vim
filetype plugin indent on
syntax on

call vundle#begin()
Plugin 'schickling/vim-bufonly'
Plugin 'tacahiroy/ctrlp-funky'
Plugin 'majutsushi/tagbar'
Plugin 'kien/ctrlp.vim'
Plugin 'fholgado/minibufexpl.vim'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-easytags'
Plugin 'Chiel92/vim-autoformat'
Plugin 'kchmck/vim-coffee-script'
Plugin 'KurtPreston/vim-autoformat-rails'
Plugin 'Shougo/neocomplcache'
Plugin 'Shougo/neocomplcache.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'ajh17/Spacegray.vim'
Plugin 'bkad/CamelCaseMotion'
Plugin 'blackgate/tropikos-vim-theme'
Plugin 'bronson/vim-trailing-whitespace'
Plugin 'chrisbra/changesPlugin'
Plugin 'chriskempson/base16-vim'
Plugin 'chriskempson/vim-tomorrow-theme'
Plugin 'flazz/vim-colorschemes'
Plugin 'godlygeek/tabular'
Plugin 'gregsexton/gitv'
Plugin 'jelera/vim-javascript-syntax'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'junegunn/vader.vim'
Plugin 'madnight/vim-swap-lines'
Plugin 'matze/vim-move'
Plugin 'mhinz/vim-signify'
Plugin 'mileszs/ack.vim'
Plugin 'mxw/vim-jsx'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'orthecreedence/void.vim'
Plugin 'pangloss/vim-javascript'
Plugin 'reedes/vim-thematic'
Plugin 'scheakur/vim-scheakur'
Plugin 'scrooloose/nerdcommenter'
Plugin 'scrooloose/nerdtree'
Plugin 'sjl/gundo.vim'
Plugin 'smancill/conky-syntax.vim'
Plugin 'suan/vim-instant-markdown'
Plugin 'szw/vim-tags'
Plugin 'textlint/textlint'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-sensible'
Plugin 'tpope/vim-surround'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'vim-scripts/a.vim'
Plugin 'vim-syntastic/syntastic'
Plugin 'w0ng/vim-hybrid'
call vundle#end()

" Functions
function! Profile() 
  :profile start profile.log
  :profile func *
  :profile file *
endfunction

function! StopProfile() 
  :profile pause
  :noautocmd qall!
endfunction

" syntastic settings
let g:syntastic_loc_list_height = 2
let g:syntastic_check_on_open = 1
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = "▲"
let g:syntastic_cpp_config_file = '~/.syntastic_includes'
let g:syntastic_cpp_check_header = 1
let g:syntastic__signs=1

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:livepreview_previewer = 'evince'
let g:LatexBox_latexmk_options = "-pvc -pdfps"
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_latexmk_async=1
let g:Tex_CompileRule_pdf = 'latexmk -pdf'

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

" Jump to the last known cursor position when opening a file.
augroup vimrc
  au!
  autocmd BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
augroup END

if has('mouse')
  set mouse=a
endif

set shell=/bin/bash 
set tabstop=4       " The width of a TAB is set to 4.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 4.
set shiftwidth=4    " Indents will have a width of 4
set softtabstop=4   " Sets the number of columns for a TAB
set expandtab       " Expand TABs to spaces

let g:SuperTabDefaultCompletionType = "<c-n>"

autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1 
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby set omnifunc=rubycomplete#Complete
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" vim airline
set laststatus=2
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 0
let g:airline_theme='molokai'
"let g:airline_theme='base16'
let g:ctrlp_status_func = {
  \ 'main': 'CtrlPStatusFunc_1',
  \ 'prog': 'CtrlPStatusFunc_2',
  \ }

let g:airline#extensions#hunks#enabled = 1
let g:tagbar_status_func = 'TagbarStatusFunc'

"augroup AutoSyntastic
  "autocmd!
  "autocmd BufWritePost *.c,*.cpp call s:syntastic()
"augroup END

"function! s:syntastic()
  "SyntasticCheck
"endfunction

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0

let g:vim_tags_auto_generate = 1

if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

map w <Plug>CamelCaseMotion_w
map b <Plug>CamelCaseMotion_b
map e <Plug>CamelCaseMotion_e

let g:formatprg_js = "js-beautify"
let g:formatprg_args_js = "-i %@"

set wildmode=longest,list,full
set wildmenu
set hidden
set cursorline
set ignorecase
set smartcase
set smartindent
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
set shiftwidth=4
set softtabstop=4
set expandtab
set formatoptions=l
set lbr
set nofoldenable
set mouse=a
set invnumber
set t_Co=256
set term=xterm-256color
set noswapfile
set nocompatible
set laststatus=2
set cmdheight=1
set noruler
set noshowcmd
let mapleader=","

nnoremap <C-j> <C-w>j
nnoremap <leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <leader>n :NERDTreeToggle<CR>
inoremap jk <ESC>
inoremap <Space> <Space><C-g>u
nnoremap <C-L> :nohl<CR><C-L>
map <C-PageUp> <C-w>wh<CR>
map <C-PageDown> <C-w>wl<CR>
nmap <S-k> <C-w>wl<CR>
nmap <S-w> :bd<CR>
" Jump faster
nmap <C-j> 4j
nmap <C-k> 4k
" Jump to definition (ctags -R)
nmap <C-b> g]1<CR>w
nmap o o<ESC>
nmap <C-k> 4k
nmap <C-a> :A<CR>
nmap <C-o> O<Esc>
nmap <C-l> :LLPStartPreview<CR>
nmap ^ $
nmap zz ZZ
nmap <C-g> <Plug>IMAP_JumpForward
" redef C-j to C-g latex packages
map <C-l> :LLPStartPreview<CR>
imap ii <Esc>
imap jj <Esc>
imap <C-g> <Plug>IMAP_JumpForward
vmap ^ $
map <D-/> <C-_><C-_>
map Y y$
map <C-s> <ESC>:w<CR>
"map <C-w> <ESC>:wq!<CR>
map <S-w> <ESC>:q!<CR>
imap <C-s> <ESC>:w<CR>
map <xCSI>[62~ <MouseDown>:

set background=dark

hi clear CursorLine
augroup CLClear
  autocmd! ColorScheme * hi clear CursorLine
augroup END

colorscheme hybrid

map <F5> :setlocal spell! spelllang=de_de,en_us<CR>
au BufNewFile,BufRead,BufEnter *.tex setlocal spell spelllang=de_de,en_gb
au BufNewFile,BufRead,BufEnter *.txt setlocal spell spelllang=de_de,en_gb
au BufNewFile,BufRead,BufEnter *.pl set filetype=prolog
au BufRead,BufNewFile,BufEnter *mutt* set filetype=mail
au FileType php set omnifunc=phpcomplete#CompletePHP
"au BufNewFile,BufRead,BufEnter   *.tex    set filetype=plaintex

autocmd FileType mail setlocal spell spelllang=de_de,en_gb
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1 
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
" autocmd VimEnter * NERDTree
" autocmd VimEnter * wincmd p

hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
cnoremap <silent> x<cr> call QuitPrompt()<cr>
nnoremap ZZ :call QuitPrompt()<cr>

fun! QuitPrompt()
   "if has("gui_running") && tabpagenr("$") == 1 && winnr("$") == 1
      let choice = confirm("Close?", "&yes\n&no", 1)
      if choice == 1 | wq | endif
   "else | wq | endif
endfun

if has("gui_running")
  "set guifont=Inconsolata\ for\ Powerline\ dz\ 17
  set guifont=Monaco\ 13

  "let g:nerdtree_tabs_open_on_console_startup=1
" Save session on quitting Vim
    "autocmd VimLeave * NERDTreeClose
    "
 "autocmd VimEnter * NERDTree
  " set guifont=Inconsolata-g\ 15
  " set guifont=Ubuntu\ Mono\ 15
  " set guifont=Consolas\ 15
  " set guifont=17
  " set guifont=Monospace\ 13

  " colorscheme twilight
  " colorscheme hybrid
  colorscheme base16-tomorrow-night
  set background=dark
   "colorscheme github
  " colorscheme molokai
  set guioptions-=T
  set guicursor=a:blinkon0 
  set guioptions-=m
  set guioptions-=T
  set guioptions-=L
  set guioptions-=l
  set guioptions-=R
  set guioptions-=r
  set guioptions-=b
  "" Adding automatons for when entering or leaving Vim
  au VimEnter * nested :call LoadSession()
  au VimLeave * NERDTreeClose
  au VimLeave * MBEClose
  au VimLeave * :call MakeSession()
  autocmd VimEnter * NERDTree
  let g:nerdtree_tabs_open_on_gui_startup=0
  let g:nerdtree_tabs_open_on_new_tab=0
  "let g:nerdtree_tabs_autofind=1
endif


highlight LineNr guibg=#1D1F21
set nuw=1

let g:NERDTreeWinPos = "left"
let php_sql_query=1
let Tlist_Use_Right_Window   = 1
let php_htmlInStrings=1
let g:acp_enableAtStartup = 0
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
" Enable heavy features.
" Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
let g:neocomplcache_enable_underbar_completion = 1

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
      \ 'default' : '',
      \ 'vimshell' : $HOME.'/.vimshell_hist',
      \ 'scheme' : $HOME.'/.gosh_completions'
      \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" neocomplcache
inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y> neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()

function! s:my_cr_function()
  return neocomplcache#smart_close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplcache#close_popup() : "\<CR>"
endfunction

" Enable omni completion.
if !exists('g:neocomplcache_force_omni_patterns')
  let g:neocomplcache_force_omni_patterns = {}
endif
let g:neocomplcache_force_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_force_omni_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplcache_force_omni_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
let g:neocomplcache_force_omni_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = ['eslint']

noremap <silent> <c-up> :call SwapUp()<CR>
noremap <silent> <c-down> :call SwapDown()<CR>

" By default, JSX syntax highlighting and indenting will
" be enabled only for files with the .jsx extension.
" If you would like JSX in .js files, add
let g:jsx_ext_required = 0

set list listchars=tab:»-,trail:·,extends:»,precedes:«

let g:session_autosave = 'yes'
let g:session_autoload = 'yes'

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

hi LineTooLong cterm=bold ctermbg=red guibg=LightYellow
match LineTooLong /\%>80v.\+/


function! MakeSession()
  let b:sessiondir = $HOME . "/.vim/sessions" . getcwd()
  if (filewritable(b:sessiondir) != 2)
    exe 'silent !mkdir -p ' b:sessiondir
    redraw!
  endif
  let b:filename = b:sessiondir . '/session.vim'
  exe "mksession! " . b:filename
endfunction

function! LoadSession()
  let b:sessiondir = $HOME . "/.vim/sessions" . getcwd()
  let b:sessionfile = b:sessiondir . "/session.vim"
  if (filereadable(b:sessionfile))
    exe 'source ' b:sessionfile
  else
    echo "No session loaded."
  endif
endfunction

noremap <C-Right>  :MBEbn<CR>
noremap <C-Left> :MBEbp<CR>

noremap <C-X> :bd<CR>
let g:miniBufExplUseSingleClick = 1


let NERDTreeShowHidden=1
"autocmd CursorHold,CursorHoldI * call NERDTreeFocus() | call g:NERDTree.ForCurrentTab().getRoot().refresh() | call g:NERDTree.ForCurrentTab().render() | wincmd w
au BufNewFile,BufRead *.coffee set filetype=coffee

let g:NERDTreeMouseMode = 3
 " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

"let g:ackprg = 'ag --nogroup --nocolor --column'
let g:ackprg = 'rg --vimgrep'
noremap <Leader>a :Ack <cword><cr>
  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
let g:ctrlp_funky_syntax_highlight = 1
nnoremap <Leader>fu :CtrlPFunky<Cr>
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
