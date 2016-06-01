"                 _|                                                        _|_|  _|            
"     _|      _|      _|_|_|  _|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
"     _|      _|  _|  _|    _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
"       _|  _|    _|  _|    _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
"         _|      _|  _|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
"                                                                                           _|  
"                                                                                       _|_|    

set smartindent
set rtp+=~/.vim/bundle/Vundle.vim
filetype plugin indent on
syntax on

call vundle#begin()
Plugin 'KurtPreston/vim-autoformat-rails'
Plugin 'scheakur/vim-scheakur'
Plugin 'w0ng/vim-hybrid'
Plugin 'orthecreedence/void.vim'
Plugin 'ajh17/Spacegray.vim'
Plugin 'fholgado/minibufexpl.vim'
Plugin 'Bling/vim-airline'
Plugin 'szw/vim-tags'
Plugin 'suan/vim-instant-markdown'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'DamienCassou/textlint'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'blackgate/tropikos-vim-theme'
Plugin 'flazz/vim-colorschemes'
Plugin 'vim-scripts/a.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'mhinz/vim-signify'
Plugin 'chrisbra/changesPlugin'
Plugin 'Shougo/neocomplcache'
Plugin 'godlygeek/tabular'
Plugin 'bkad/CamelCaseMotion'
Plugin 'reedes/vim-thematic'
Plugin 'Chiel92/vim-autoformat'
Plugin 'Shougo/neocomplcache.vim'
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

" scrooloose/syntastic settings 
let g:syntastic_always_populate_loc_list = 0
let g:syntastic_loc_list_height = 2
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 1
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = "▲"

"let g:syntastic_cpp_include_dirs = ['/usr/include/qt4/QtGui','/usr/include/qt4/QtCore', 'includes', 'headers']
let g:syntastic_cpp_config_file = '~/.syntastic_includes'

let g:syntastic_cpp_check_header = 1
let g:syntastic__signs=1
"let g:syntastic_quiet_warnings=1

let g:livepreview_previewer = 'evince'
let g:LatexBox_latexmk_options = "-pvc -pdfps"
let g:LatexBox_latexmk_preview_continuously=1
let g:LatexBox_latexmk_async=1
let g:Tex_CompileRule_pdf = 'latexmk -pdf'

silent! iunmap (
silent! iunmap )
silent! iunmap {
silent! iunmap }

"autocmd BufNewFile,BufWritePost,BufRead *.tex set makeprg=pdflatex\ %\ &&\ xdg-open\ %:r.pdf
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*

augroup mySyntastic
  au!
  au FileType tex let b:syntastic_mode = "passive"
augroup END

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

" MiniBufExpl Colors
hi MBENormal               guifg=#808080 guibg=fg
hi MBEChanged              guifg=#CD5907 guibg=fg
hi MBEVisibleNormal        guifg=#5DC2D6 guibg=fg
hi MBEVisibleChanged       guifg=#F1266F guibg=fg
hi MBEVisibleActiveNormal  guifg=#A6DB29 guibg=fg
hi MBEVisibleActiveChanged guifg=#F1266F guibg=fg

set shell=/bin/bash 
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
let g:airline_theme='molokai'
let g:airline_powerline_fonts = 1 

let g:ctrlp_status_func = {
  \ 'main': 'CtrlPStatusFunc_1',
  \ 'prog': 'CtrlPStatusFunc_2',
  \ }

"let g:airline_section_b = 'asdasdasdsad'
let g:airline#extensions#hunks#enabled = 1
let g:tagbar_status_func = 'TagbarStatusFunc'

augroup AutoSyntastic
  autocmd!
  autocmd BufWritePost *.c,*.cpp call s:syntastic()
augroup END

function! s:syntastic()
  SyntasticCheck
endfunction

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
set background=light
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
let g:miniBufExplMapCTabSwitchBufs = 1
let mapleader=","
"let g:SuperTabDefaultCompletionType = "<C-X><C-O>"
"let g:livepreview_previewer = 'zathura'
"let g:auto_save = 1 


nnoremap <C-j> <C-w>j
nnoremap <leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <leader>n :NERDTreeToggle<CR>
inoremap jk <ESC>
nnoremap <C-L> :nohl<CR><C-L>
" keymaps
" control h, j, k, l tab navigation
" nmap <S-h> <C-w>h			
" nmap <S-j> <C-w>wh<CR>
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
imap <C-s> <ESC>:w<CR>i
map <xCSI>[62~ <MouseDown>:
"map <S-d> <S-d><ESC>
"map <S-a> <S-a><ESC>


""""""plugin settings"""""""""""""""""""""
hi clear CursorLine
augroup CLClear
  autocmd! ColorScheme * hi clear CursorLine
augroup END

"colorscheme molokai
"colorscheme genericdc
colorscheme default
"colorscheme twilight 
"colorscheme wombat256mod
"colorscheme atom-dark-256
"

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

" highlight LineNr ctermfg=59
highlight LineNr ctermfg=236

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
  set guifont=Inconsolata\ for\ Powerline\ 17
  " set guifont=Monaco\ 13
  " set guifont=Inconsolata-g\ 15
  " set guifont=Ubuntu\ Mono\ 15
  " set guifont=Consolas\ 17
  " set guifont=17
  " set guifont=Monospace\ 13
  
  colorscheme twilight
  " colorscheme molokai
  " colorscheme github

 
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


"hi Comment      ctermfg=7
"hi VertSplit	ctermfg=0 ctermbg=none
"hi StatusLine	ctermfg=0 ctermbg=none
"hi StatusLineNC	ctermfg=0 ctermbg=none
"hi Folded ctermbg=0 ctermfg=8
"hi Pmenu ctermfg=7 ctermbg=0
"hi PmenuSel ctermfg=0 ctermbg=15
"hi LineNr ctermfg=0 ctermbg=none
"hi CursorLine ctermfg=none ctermbg=none cterm=none
"hi CursorLineNr ctermfg=none ctermbg=0
"hi CursorColumn ctermfg=none ctermbg=0
"hi SignColumn ctermbg=none
"hi SyntasticErrorSign ctermfg=1 ctermbg=none
hi SyntasticWarningSign ctermfg=3 ctermbg=none
hi SyntasticStyleErrorSign ctermfg=1 ctermbg=none
hi SyntasticStyleWarningSign ctermfg=3 ctermbg=none
hi SyntasticErrorLine ctermfg=none ctermbg=none
hi SyntasticWarningLine ctermfg=none ctermbg=none
hi SyntasticStyleErrorLine ctermfg=none ctermbg=none
hi SyntasticStyleWarningLine ctermfg=none ctermbg=none
"hi SpellBad ctermfg=0 ctermbg=3
"hi SpellCap ctermfg=0 ctermbg=1
"hi LineNr ctermfg=59  ctermbg=NONE
hi CursorLine cterm=bold term=bold
hi Statement ctermfg=3
hi String ctermfg=15
"hi String ctermfg=15
"hi Identifier ctermfg=59
let g:NERDTreeWinPos = "left"
let php_sql_query=1                                                                                        
let Tlist_Use_Right_Window   = 1
let php_htmlInStrings=1
let s:green = '#a2a96f'
let s:orange= '#a2a96f'
let s:lightgreen = '#c2c98f'
let colors_name = "default"
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
