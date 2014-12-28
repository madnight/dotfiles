"                 _|                                                        _|_|  _|            
"     _|      _|      _|_|_|  _|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
"     _|      _|  _|  _|    _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
"       _|  _|    _|  _|    _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
"         _|      _|  _|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
"                                                                                           _|  
"                                                                                       _|_|    
filetype indent plugin on
syntax on
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'Chiel92/vim-autoformat'
Bundle 'croaky/vim-colors-github'
call vundle#end()            
filetype plugin indent on    

let g:formatprg_js = "js-beautify"
let g:formatprg_args_js = "-i %@"
set hidden
set wildmenu
set showcmd
set hlsearch
set ignorecase
set smartcase
set smartindent
""set autoindent
set backspace=indent,eol,start
set si
set nostartofline
set ruler
set clipboard=unnamedplus
set laststatus=2
set confirm
set t_vb=
set mouse=a
set cmdheight=2
set notimeout ttimeout ttimeoutlen=200
set pastetoggle=<F11>
set shiftwidth=2
set tw=79
set softtabstop=2
set expandtab
set formatoptions=l
set lbr
set wrap
set nofoldenable
set cursorline
set background=light
set mouse=a
set invnumber
set t_Co=256
set term=xterm-256color
set laststatus=2
set noswapfile
set nocompatible

let g:livepreview_previewer = 'zathura'
let g:auto_save = 1 
let g:molokai_original = 1
let g:rehash256 = 1
let mapleader="-"
","let g:EasyMotion_leader_key = 'z'


"""""""basic keymaps""""""""""""""""

nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l
nmap <C-o> O<Esc>
nmap <C-s> :w<CR>
nmap <C-l> :LLPStartPreview<CR>
nmap ^ $
nmap <C-a> <C-u>
nmap zz ZZ
nnoremap <leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <C-L> :nohl<CR><C-L>
imap <C-s> <ESC>:w<CR>a
imap <C-l> :LLPStartPreview<CR>
imap ii <Esc>
imap jj <Esc>
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
vmap ^ $
map <D-/> <C-_><C-_>
map Y y$
map <xCSI>[62~ <MouseDown>:
inoremap jk <ESC>
filetype on

"nmap <C-l> :set invnumber<CR>

""""""plugin settings"""""""""""""""""""""


call pathogen#infect() 
""colorscheme molokai
""colorscheme genericdc
colorscheme default
""colorscheme twilight 
""colorscheme wombat256mod
""colorscheme atom-dark-256
au BufNewFile,BufRead,BufEnter   *.tex     setlocal spell    spelllang=de_de
au BufNewFile,BufRead,BufEnter   *.txt     setlocal spell    spelllang=de_de
au BufNewFile,BufRead,BufEnter   *.pl    set filetype=prolog
au BufRead,BufNewFile,BufEnter *mutt* set filetype=mail
"au BufNewFile,BufRead,BufEnter   *.tex    set filetype=plaintex
au FileType php set omnifunc=phpcomplete#CompletePHP
""au FileType php :TlistToggle
let Tlist_Use_Right_Window   = 1


autocmd FileType mail setlocal spell spelllang=de_de

"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p

hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

highlight LineNr ctermfg=59

highlight LineNr ctermfg=236
""set textwidth=72
""set wrapmargin=72
""set tw=72
""set statusline=%F%m%r%h%w[%L][%{&ff}]%y[%p%%][%04l,%04v]
" Nice window title

if has("gui_running")
""  set guifont=Monaco\ 13
"J"  set guifont=Inconsolata-g\ 15
  ""  set guifont=Ubuntu\ Mono\ 15
   set guifont=Consolas\ 15
  ""set guifont=Monospace\ 13
  ""    colorscheme molokai
  ""colorscheme twilight
  colorscheme github
  set guioptions-=T
  set guicursor=a:blinkon0 
  set guioptions-=m
  set guioptions-=T
  set guioptions-=L
  set guioptions-=l
  set guioptions-=R
  set guioptions-=r
  set guioptions-=b    
  set guioptions+=a
endif



hi Comment      ctermfg=7
hi VertSplit	ctermfg=0 ctermbg=none
hi StatusLine	ctermfg=0 ctermbg=none
hi StatusLineNC	ctermfg=0 ctermbg=none
hi Folded ctermbg=0 ctermfg=8
hi Pmenu ctermfg=7 ctermbg=0
hi PmenuSel ctermfg=0 ctermbg=15
hi LineNr ctermfg=0 ctermbg=none
hi CursorLine ctermfg=none ctermbg=none cterm=none
""hi CursorLineNr ctermfg=none ctermbg=0
hi CursorColumn ctermfg=none ctermbg=0
" Syntax checker colors
highlight SignColumn ctermbg=none
hi SyntasticErrorSign ctermfg=1 ctermbg=none
hi SyntasticWarningSign ctermfg=3 ctermbg=none
hi SyntasticStyleErrorSign ctermfg=1 ctermbg=none
hi SyntasticStyleWarningSign ctermfg=3 ctermbg=none
hi SyntasticErrorLine ctermfg=none ctermbg=none
hi SyntasticWarningLine ctermfg=none ctermbg=none
hi SyntasticStyleErrorLine ctermfg=none ctermbg=none
hi SyntasticStyleWarningLine ctermfg=none ctermbg=none
""hi SpellBad ctermfg=0 ctermbg=3
""hi SpellCap ctermfg=0 ctermbg=1


" You might also find this useful
" PHP Generated Code Highlights (HTML & SQL)                                              

let php_sql_query=1                                                                                        
let php_htmlInStrings=1

" Default Color Modifications
""hi LineNr ctermfg=59  ctermbg=NONE
hi CursorLine cterm=bold term=bold
hi Statement ctermfg=1
hi Identifier ctermfg=59
let s:green = '#a2a96f'
let s:lightgreen = '#c2c98f'
let colors_name = "default"
"
