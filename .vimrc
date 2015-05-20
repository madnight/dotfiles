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

set hidden
set wildmenu
set showcmd
set hlsearch
set ignorecase
set smartcase
set smartindent
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
let g:updatetime = 200
let g:molokai_original = 1
let g:rehash256 = 1
let mapleader="-"
let g:formatprg_js = "js-beautify"
let g:formatprg_args_js = "-i %@"
let php_sql_query=1                                                                                        
let php_htmlInStrings=1
let Tlist_Use_Right_Window   = 1
let s:green = '#a2a96f'
let s:lightgreen = '#c2c98f'
let colors_name = "default"
let g:SuperTabDefaultCompletionType = "<C-X><C-O>"

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
"nmap <C-l> :set invnumber<CR>
nnoremap <leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <leader>n :NERDTreeToggle<CR>
imap <C-s> <ESC>:w<CR>a
imap <C-l> :LLPStartPreview<CR>
imap ii <Esc>
imap jj <Esc>:w<CR>
vmap ^ $
map <D-/> <C-_><C-_>
map Y y$
map <xCSI>[62~ <MouseDown>:
map <F5> :setlocal spell! spelllang=en_us,de_de<CR>
nmap <F4> :NERDTreeToggle<CR>:TagbarToggle<CR> 
map <C-p> <S-p>
inoremap jk <ESC>
autocmd TextChanged <buffer> w
filetype on
""""""plugin settings"""""""""""""""""""""

call pathogen#infect() 
"
""colorscheme molokai
""colorscheme genericdc
colorscheme default
"colorscheme Tommorow Night
""colorscheme twilight 
""colorscheme wombat256mod
""colorscheme atom-dark-256

au BufNewFile,BufRead,BufEnter   *.tex     setlocal spell    spelllang=en_us,de_de
au BufNewFile,BufRead,BufEnter   *.txt     setlocal spell    spelllang=en_us,de_de
au BufNewFile,BufRead,BufEnter   *.pl    set filetype=prolog
au BufRead,BufNewFile,BufEnter *mutt* set filetype=mail
au FileType php set omnifunc=phpcomplete#CompletePHP

autocmd FileType mail setlocal spell spelllang=en_us,de_de

"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p

hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

""set textwidth=72
""set wrapmargin=72
""set tw=72

if has("gui_running")
"" set guifont=Monaco\ 13
"" set guifont=Inconsolata-g\ 15
"" set guifont=Ubuntu\ Mono\ 15
   set guifont=Consolas\ 15
"   set guifont=Monaco\ 14
"" set guifont=Monospace\ 13
"" colorscheme molokai
   colorscheme twilight
""   colorscheme vydark
"" colorscheme github
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
highlight LineNr ctermfg=59
highlight LineNr ctermfg=236
hi CursorColumn ctermfg=none ctermbg=0
highlight SignColumn ctermbg=none
hi SyntasticErrorSign ctermfg=1 ctermbg=none
hi SyntasticWarningSign ctermfg=3 ctermbg=none
hi SyntasticStyleErrorSign ctermfg=1 ctermbg=none
hi SyntasticStyleWarningSign ctermfg=3 ctermbg=none
hi SyntasticErrorLine ctermfg=none ctermbg=none
hi SyntasticWarningLine ctermfg=none ctermbg=none
hi SyntasticStyleErrorLine ctermfg=none ctermbg=none
hi SyntasticStyleWarningLine ctermfg=none ctermbg=none

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 0
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
