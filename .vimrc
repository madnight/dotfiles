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
  autocmd BufReadPost *
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

nnoremap <C-j> <C-w>j
nnoremap K :Ack! '<C-r><C-w>'<cr>
nnoremap <Leader>sb :CtrlPBuffer<CR>
nnoremap <leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <C-L> :nohl<CR><C-L>
" remove trailing white spaces
nnoremap <Leader>rw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>dw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>fu :CtrlPFunky<Cr>
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
nnoremap <silent> <C-p> :CtrlP<CR>
nnoremap <silent> <C-r> :earlier<CR>
nnoremap <silent> <C-e> :WinResizerStartResize<CR>
nnoremap ZZ :call QuitPrompt()<cr>

cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
cnoremap <silent> x<cr> call QuitPrompt()<cr>

noremap <Leader>a :Ack <cword><cr>
noremap <Leader>s :Ack
noremap <C-X> :bd<CR>
noremap <silent> <c-up> :call SwapUp()<CR>
noremap <silent> <c-down> :call SwapDown()<CR>
noremap <C-Right>  :MBEbn<CR>
noremap <C-Left> :MBEbp<CR>
noremap <Leader>rn :call NumberToggle()<CR>

" fugitive shortcuts
noremap <Leader>gs :Gstatus<cr>
noremap <Leader>gc :Gcommit<cr>
noremap <Leader>ga :Gwrite<cr>
noremap <Leader>gl :Glog<cr>
noremap <Leader>gd :Gdiff<cr>
noremap <Leader>gb :Gblame<cr>

map <C-PageUp> <C-w>wh<CR>
map <C-PageDown> <C-w>wl<CR>
map w <Plug>CamelCaseMotion_w
map b <Plug>CamelCaseMotion_b
map e <Plug>CamelCaseMotion_e

inoremap jk <ESC>

nmap s <Plug>(easymotion-bd-f)
nmap <Leader>rv <ESC>:so ~/.vimrc<CR>
nmap <Leader>v <ESC>:so ~/.vimrc<CR>
nmap ya y$
nmap <S-k> <C-w>wl<CR>
nmap <S-w> :bd<CR>
" Jump faster
nmap <C-j> 4j
nmap <C-h> <C-w>h
nmap <M-l> <C-w>l
" Alt / Mod Key (A-,M-) := <ESC>
nmap <ESC>l <C-w>l
nmap <ESC>h <C-w>h
nmap <ESC>j <C-w>j
nmap <ESC>k <C-w>k
" Jump to definition (ctags -R)
nmap <C-b> <C-]>
nmap o o<ESC>
nmap <C-k> 4k
nmap <C-a> :A<CR>
nmap <C-o> O<Esc>
nmap ^ $
nmap zz ZZ
nmap <C-g> <Plug>IMAP_JumpForward

imap ii <Esc>
imap jj <Esc>
imap <C-s> <ESC>:w<CR>
imap <C-g> <Plug>IMAP_JumpForward

vmap ^ $
vnoremap K :<C-u>call <sid>VisualAck()<cr>

map <D-/> <C-_><C-_>
map Y y$
map <C-s> <ESC>:w<CR>
map <S-w> <ESC>:q!<CR>
map <F5> :setlocal spell! spelllang=de_de,en_us<CR>

set background=dark

hi clear CursorLine
augroup CLClear
  autocmd! ColorScheme * hi clear CursorLine
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

hi clear SpellBad
hi SpellBad cterm=underline ctermfg=red

highlight LineNr guibg=#1D1F21
set nuw=1

let g:livepreview_previewer = 'evince'
let g:LatexBox_latexmk_options = "-pvc -pdfps"
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_latexmk_async=1
let g:Tex_CompileRule_pdf = 'latexmk -pdf'
" lilydjwg/colorizer is inefficient for large files
let g:colorizer_maxlines = 200
let php_sql_query=1
let Tlist_Use_Right_Window   = 1
let php_htmlInStrings=1
let g:acp_enableAtStartup = 0
" add jsx syntax highlights for .js files
let g:jsx_ext_required = 0
" overcome limit imposed by max height
let g:ackprg = 'rg --vimgrep'

" rg is so fast that CtrlP doesn't need to cache
let g:ctrlp_use_caching = 0
let g:ctrlp_funky_syntax_highlight = 1
let g:ctrlp_match_window = 'results:100'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_user_command = 'rg %s --files --color never'

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#buffer_min_count =2
let g:airline_theme='molokai'
let g:airline#extensions#tabline#enabled = 1
" performance optimization
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#branch#enabled = 0
let g:airline#extensions#whitespace#enabled = 0

let g:unite_force_overwrite_statusline = 0
let g:vimfiler_force_overwrite_statusline = 0
let g:vimshell_force_overwrite_statusline = 0
let g:vim_tags_auto_generate = 1
let g:formatprg_js = "js-beautify"
let g:formatprg_args_js = "-i %@"

let g:NERDTreeMinimalUI = 1
let g:NERDTreeShowHidden=1
let g:NERDTreeWinPos = "left"
let g:NERDTreeMouseMode = 3
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1
let g:NERDTreeWinSize = 40
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Add your own custom formats or override the defaults
let g:NERDTrimTrailingWhitespace = 1

let g:winresizer_horiz_resize = 1
let g:vim_markdown_preview_github=1

let g:hardtime_default_on = 0
let g:hardtime_maxcount = 1000

let g:UltiSnipsExpandTrigger="<C-l>"
let g:ctrlp_show_hidden = 1

let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
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

highlight OverLength ctermbg=red ctermfg=white guibg=#592929
match OverLength /\%81v.\+/

source ~/.private_vimrc

