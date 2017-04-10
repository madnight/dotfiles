"                 |                                                        _|_|  _|
"     _|      _|      _|_|_|  _|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|
"     _|      _|  _|  _|    _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|
"       _|  _|    _|  _|    _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|
"         _|      _|  _|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|
"                                                                                           _|
"                                                                                       _|_|

function! BuildYCM(info)
  if a:info.status == 'installed' || a:info.force
    !./install.py
  endif
endfunction

call plug#begin()
" a code-completion engine for Vim
Plug 'Valloric/YouCompleteMe', { 'on': [], 'do': function('BuildYCM') }
" improved PHP omni-completion, based on the default phpcomplete.vim
Plug 'shawncplus/phpcomplete.vim', { 'for': 'php' }
" easymotion provides a much simpler way to use some motions in vim
Plug 'easymotion/vim-easymotion'
" editorconfig define consistent coding styles in different editors
Plug 'editorconfig/editorconfig-vim'
" resizer mode ctrl-e
Plug 'simeji/winresizer', { 'on': ['WinResizerStartResize'] }
" Peekaboo extends " @ normal mode and <CTRL-R> insert mode see the contents of the registers
Plug 'junegunn/vim-peekaboo'
" hardtime helps you break that annoying habit vimmers have
Plug 'takac/vim-hardtime', { 'on': ['HardTimeOn'] }
" Track the engine.
Plug 'SirVer/ultisnips', { 'on': [] }
" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets', { 'on': [] }
" incsearch.vim incrementally highlights ALL pattern matches
Plug 'haya14busa/incsearch.vim'
" an ack.vim alternative mimics Ctrl-Shift-F (search with context)
Plug 'dyng/ctrlsf.vim', {'on': ['CtrlSF']}
" shows a git diff in the 'gutter' (sign column) // disabled performance reasons
" Plug 'airblade/vim-gitgutter'
" camel case jumps with w
Plug 'bkad/CamelCaseMotion'
" show trailing whitespaces
Plug 'bronson/vim-trailing-whitespace'
" provide easy code formatting in Vim by integrating existing code formatters
Plug 'Chiel92/vim-autoformat'
" enhanced vim diff
Plug 'chrisbra/vim-diff-enhanced'
" base16 colorscheme
Plug 'chriskempson/base16-vim'
" fuzzy file, buffer, mru, tag, etc finder
Plug 'ctrlpvim/ctrlp.vim', {'on': ['CtrlP', 'CtrlPMixed', 'CtrlPMRU']}
" small bar with clickable tabs (buffers)
Plug 'fholgado/minibufexpl.vim'
" a big collection of colorscheme
Plug 'flazz/vim-colorschemes'
" vim script for text filtering and alignment
Plug 'godlygeek/tabular'
" gitk for vim
Plug 'gregsexton/gitv',  {'on': ['Gitv']}
" open markdown preview in browser
Plug 'JamshedVesuna/vim-markdown-preview', { 'for': 'markdown' }
" syntax highlighting, matching rules and mappings for the original Markdown and extensions
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' }
" HTML5 + inline SVG omnicomplete function, indent and syntax for vim
Plug 'othree/html5.vim', { 'for': 'markdown' }
" distinct highlighting of keywords vs values, JSON-specific (non-JS) warnings, quote concealing
Plug 'elzr/vim-json', { 'for': 'json' }
" this vim bundle adds syntax highlighting, indenting and autocompletion for the dynamic stylesheet language LESS.
Plug 'groenewege/vim-less', { 'for': 'less' }
" runtime files for Haml, Sass, and SCSS that ship with vim
Plug 'tpope/vim-haml', { 'for' : ['haml','sass','scss','css'] }
" adds some more stuff that I find useful, including all of my notes and customizations.
Plug 'derekwyatt/vim-scala', { 'for': 'scala' }
" add go support for vim
Plug 'fatih/vim-go', { 'for': 'go' }
" offical rust plugin
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
" highlight variables differently
Plug 'jaxbot/semantic-highlight.vim'
" enhanced javascript synxtax highlighting
Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
" improved nerdtree side panel (more ide like)
Plug 'jistr/vim-nerdtree-tabs', { 'on':  'NERDTreeToggle' }
" css3 syntax highlight
Plug 'hail2u/vim-css3-syntax', { 'for': 'css' }
" java syntax highlight
Plug 'artur-shaik/vim-javacomplete2', {'for': 'java'}
" nginx syntax highlight
Plug 'evanmiller/nginx-vim-syntax',  { 'for': 'nginx' }
" wrapper for fzf a command line fuzzy finder
Plug 'junegunn/fzf.vim', { 'do': 'yes \| ./install' }
" lightweight gitv (if gitv is too slow)
Plug 'junegunn/gv.vim', {'on': ['GV']}
" coffeescript syntax support
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
" colorize hex codes this plugin is still inefficient for large files
Plug 'lilydjwg/colorizer', { 'for': 'css' }
" debugger for php, python and other languages
Plug 'joonty/vdebug', {'on': ['VdebugStart']}
" allow atom like line swapping with arrow keys
Plug 'madnight/vim-swap-lines'
" ide like tagbar that lists all function of a class / file
" Plug 'majutsushi/tagbar', {'on' : 'LdTagbar'} // disabled performance reasons
" move selected text up and down
Plug 'matze/vim-move'
" in file text string search of current dir with super fast rg
Plug 'mileszs/ack.vim', { 'on': 'Ack' }
" show mru (most recently used) files
Plug 'vim-scripts/mru.vim', { 'on': 'Mru' }
" vim support for react jsx
Plug 'mxw/vim-jsx', { 'for': 'javascript' }
" wisely add end in ruby
Plug 'tpope/vim-endwise', { 'for': [ 'ruby'] }
" enhanced cpp support
Plug 'octol/vim-cpp-enhanced-highlight', { 'for': 'cpp' }
" code completion for python
Plug 'davidhalter/jedi-vim', { 'for': 'python' }
" vim syntax file for Docker's Dockerfile and snippets for snipMate
Plug 'ekalinin/Dockerfile.vim', { 'for': 'Dockerfile' }
" vim javascript support
Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
" nerdtree icons
" Plug 'ryanoasis/vim-devicons', { 'on':  'NERDTreeToggle' } // disabled performance reasons
" delete all the buffers except the current buffer :Bonly
Plug 'schickling/vim-bufonly', { 'on':  'Bonly' }
" an ide like file explorer
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
" vim plugin to visualize your vim undo tree.
Plug 'sjl/gundo.vim', { 'on':  'Gundo' }
" global replace
Plug 'skwp/greplace.vim'
" conky syntax highlighting
Plug 'smancill/conky-syntax.vim'
" markdown preview
Plug 'suan/vim-instant-markdown', { 'for': 'markdown' }
" ctags generator
Plug 'szw/vim-tags'
" jump to functions
Plug 'tacahiroy/ctrlp-funky', {'on': 'CtrlPFunky'}
" select increasingly greater region of text
Plug 'terryma/vim-expand-region'
" colorized filenames in nerdtree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on':  'NERDTreeToggle' }
" comment out stuff via shortcut
Plug 'tpope/vim-commentary', { 'on':  'Commentary' }
" git wrapper that should be illegal
Plug 'tpope/vim-fugitive'
" minimal common sense vim tweaks
Plug 'tpope/vim-sensible'
" automatically adjusts 'shiftwidth' and 'expandtab' heuristically based
Plug 'tpope/vim-sleuth'
" add parentheses arround current word or sentence
Plug 'tpope/vim-surround'
" open url in browser
Plug 'tyru/open-browser.vim'
" vim status line
Plug 'vim-airline/vim-airline'
" vim status line themes
Plug 'vim-airline/vim-airline-themes'
" jump between source and header (.cpp -> .h)
Plug 'vim-scripts/a.vim', {'on': 'A'}
" hybrid theme https://i.imgur.com/M3Qthm4.png
Plug 'w0ng/vim-hybrid'
" syntastic replacement that uses background threads for syntax check
Plug 'w0rp/ale'
" automated tag file generation and syntax highlighting of tags in vim
" Plug 'xolox/vim-easytags' // disabled performance reasons
" miscellaneous auto-load vim scripts
Plug 'xolox/vim-misc'
" nerdtree git support
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on':  'NERDTreeToggle' }
call plug#end()

set smartindent
filetype plugin indent on
syntax on

nmap s <Plug>(easymotion-bd-f)

nmap <Leader>c<space> <ESC>:Commentary<CR>

nmap ya y$

nnoremap <silent> <C-p> :CtrlP<CR>
nnoremap <silent> <C-r> :CtrlPMRU<CR>

nnoremap <silent> <C-e> :WinResizerStartResize<CR>

command! CtrlPFunky call plug#load('ctrlp.vim', 'ctrlp-funky') | CtrlPFunky

augroup load_us_ycm
  autocmd!
  autocmd InsertEnter * call plug#load('ultisnips', 'YouCompleteMe', 'vim-snippets')
                     \| autocmd! load_us_ycm
augroup END

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

let g:livepreview_previewer = 'evince'
let g:LatexBox_latexmk_options = "-pvc -pdfps"
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_latexmk_async=1
let g:Tex_CompileRule_pdf = 'latexmk -pdf'

" lilydjwg/colorizer is inefficient for large files
let g:colorizer_maxlines = 200

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
set shiftwidth=4    " Indents will have a width of 4
set softtabstop=4   " Sets the number of columns for a TAB
set expandtab       " Expand TABs to spaces

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
nnoremap <C-L> :nohl<CR><C-L>
map <C-PageUp> <C-w>wh<CR>
map <C-PageDown> <C-w>wl<CR>
nmap <S-k> <C-w>wl<CR>
nmap <S-w> :bd<CR>

" Jump faster
nmap <C-j> 4j
nmap <C-h> <C-w>h
" nmap <C-l> <C-w>l

nmap <M-l> <C-w>l

" Alt / Mod Key (A-,M-) := <ESC>
nmap <ESC>l <C-w>l
nmap <ESC>h <C-w>h
nmap <ESC>j <C-w>j
nmap <ESC>k <C-w>k

" Jump to definition (ctags -R)
nmap <C-b> <C-]>

"noremap <C-b>
nmap o o<ESC>
nmap <C-k> 4k
nmap <C-a> :A<CR>
nmap <C-o> O<Esc>
nmap ^ $
nmap zz ZZ
nmap <C-g> <Plug>IMAP_JumpForward
" redef C-j to C-g latex packages
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
"au FileType php set omnifunc=phpcomplete#CompletePHP
"au BufNewFile,BufRead,BufEnter   *.tex    set filetype=plaintex

autocmd FileType mail setlocal spell spelllang=de_de,en_gb
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1

hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
cnoremap <silent> x<cr> call QuitPrompt()<cr>
nnoremap ZZ :call QuitPrompt()<cr>

fun! QuitPrompt()
      let choice = confirm("Close?", "&yes\n&no", 1)
      if choice == 1 | wq | endif
endfun

if has("gui_running")
  set guifont=Monaco\ 15
  map <leader>r :NERDTreeFind<cr>
  " set guifont=Inconsolata-g\ 15
  " set guifont=Ubuntu\ Mono\ 15
  " set guifont=Consolas\ 15
  " set guifont=Monospace\ 13

  " colorscheme twilight
  " colorscheme hybrid
  colorscheme base16-tomorrow-night
  set background=dark
  " colorscheme github
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
endif

highlight LineNr guibg=#1D1F21
set nuw=1

let g:NERDTreeWinPos = "left"
let php_sql_query=1
let Tlist_Use_Right_Window   = 1
let php_htmlInStrings=1
let g:acp_enableAtStartup = 0

noremap <silent> <c-up> :call SwapUp()<CR>
noremap <silent> <c-down> :call SwapDown()<CR>

let g:jsx_ext_required = 0          " add jsx syntax highlights for .js files

set list listchars=tab:»-,trail:·,extends:»,precedes:«

let g:session_autosave = 'yes'
let g:session_autoload = 'yes'

let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

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
au BufNewFile,BufRead *.coffee set filetype=coffee

let g:NERDTreeMouseMode = 3

set grepprg=rg\ --color\ never\ --line-number\ --no-heading
let g:ctrlp_user_command = 'rg %s --files --color never'
let g:ctrlp_use_caching = 0
let g:ctrlp_match_window = 'results:100' " overcome limit imposed by max height
let g:ackprg = 'rg --vimgrep'

noremap <Leader>a :Ack <cword><cr>
noremap <Leader>s :Ack

let g:ctrlp_use_caching = 0                     " rg is so fast that CtrlP doesn't need to cache
let g:ctrlp_funky_syntax_highlight = 1
nnoremap <Leader>fu :CtrlPFunky<Cr>
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>

highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

let NERDTreeMinimalUI = 1
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1

let g:NERDTreeWinSize = 40
let g:winresizer_horiz_resize = 1

let vim_markdown_preview_github=1
let g:hardtime_default_on = 0
let g:hardtime_maxcount = 1000

let g:ycm_semantic_triggers =  {
  \   'c' : ['->', '.'],
  \   'objc' : ['->', '.', 're!\[[_a-zA-Z]+\w*\s', 're!^\s*[^\W\d]\w*\s',
  \             're!\[.*\]\s'],
  \   'ocaml' : ['.', '#'],
  \   'cpp,objcpp' : ['->', '.', '::'],
  \   'perl' : ['->'],
  \   'php' : ['->', '::'],
  \   'cs,java,javascript,typescript,d,python,perl6,scala,vb,elixir,go' : ['.'],
  \   'ruby' : ['.', '::'],
  \   'lua' : ['.', ':'],
  \   'erlang' : [':'],
  \ }
let g:UltiSnipsExpandTrigger="<C-l>"
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ctrlp_show_hidden = 1

source ~/.private_vimrc

" fugitive shortcuts
noremap <Leader>gs :Gstatus<cr>
noremap <Leader>gc :Gcommit<cr>
noremap <Leader>ga :Gwrite<cr>
noremap <Leader>gl :Glog<cr>
noremap <Leader>gd :Gdiff<cr>
noremap <Leader>gb :Gblame<cr>

" performance optimization
let g:airline#extensions#branch#enabled = 0
let g:airline#extensions#whitespace#enabled = 0
