 "      ____  _             _
 "      |  _ \| |_   _  __ _(_)_ __  ___
 "      | |_) | | | | |/ _` | | '_ \/ __|
 "      |  __/| | |_| | (_| | | | | \__ \
 "      |_|   |_|\__,_|\__, |_|_| |_|___/
 "                     |___/

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
" ctrlp improved result matcher
Plug 'FelikZ/ctrlp-py-matcher'
" Plug 'nixprime/cpsm'
" generate fancy tmux status lines (airline like)
" Plug 'edkolev/tmuxline.vim',  {'on': ['Tmuxline']}
" autocomplete things that you see in other terminal
Plug 'wellle/tmux-complete.vim', { 'on': [] }
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
" list of JavaScript ES6 snippets and syntax highlighting for vim
Plug 'isRuslan/vim-es6', { 'for': 'javascript' }
" improved nerdtree side panel (more ide like)
Plug 'jistr/vim-nerdtree-tabs', { 'on':  'NERDTreeToggle' }
" css3 syntax highlight
Plug 'hail2u/vim-css3-syntax', { 'for': 'css' }
" java syntax highlight
Plug 'artur-shaik/vim-javacomplete2', {'for': 'java'}
" nginx syntax highlight
Plug 'evanmiller/nginx-vim-syntax',  { 'for': 'nginx' }
" wrapper for fzf a command line fuzzy finder
" Plug 'junegunn/fzf.vim', { 'do': 'yes \| ./install' }
" lightweight gitv (if gitv is too slow)
Plug 'junegunn/gv.vim', {'on': ['GV']}
" coffeescript syntax support
Plug 'kchmck/vim-coffee-script', { 'for': 'coffee' }
" colorize hex codes this plugin is still inefficient for large files
Plug 'gko/vim-coloresque', { 'for': 'css' }
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
Plug 'christoomey/vim-tmux-navigator'
" vim status line themes
Plug 'vim-airline/vim-airline-themes'
" jump between source and header (.cpp -> .h)
Plug 'vim-scripts/a.vim', {'on': 'A'}
" hybrid theme https://i.imgur.com/M3Qthm4.png
Plug 'w0ng/vim-hybrid'
" syntastic replacement that uses background threads for syntax check
Plug 'w0rp/ale'
" syntax highlighting for tmux
Plug 'tmux-plugins/vim-tmux', { 'for': 'tmux' }
" automated tag file generation and syntax highlighting of tags in vim
" Plug 'xolox/vim-easytags' // disabled performance reasons
" miscellaneous auto-load vim scripts
Plug 'xolox/vim-misc'
" nerdtree git support
Plug 'Xuyuanp/nerdtree-git-plugin', { 'on':  'NERDTreeToggle' }
" distraction-free writing in Vim
Plug 'junegunn/goyo.vim', {'on': 'Goyo'}
" search on google
Plug 'szw/vim-g', {'on': 'Google'}
" comment out stuff via shortcut
Plug 'scrooloose/nerdcommenter'
" toggles between relative and absolute line numbers automatically
Plug 'jeffkreeftmeijer/vim-numbertoggle'

nmap  <Leader>cw <ESC>
" cvim / vimium like window choosing by hinting
Plug 't9md/vim-choosewin', {'on': 'ChooseWin'}
call plug#end()

augroup load_us_ycm
  autocmd!
  autocmd InsertEnter * call plug#load('ultisnips', 'YouCompleteMe', 'vim-snippets', 'tmux-complete.vim')
                     \| autocmd! load_us_ycm
augroup END

function! s:VisualAck()
  let temp = @"
  normal! gvy
  let escaped_pattern = escape(@", "[]().*")
  let @" = temp
  execute "Ack! '" . escaped_pattern . "'"
endfunction

nnoremap K :Ack! '<C-r><C-w>'<cr>
vnoremap K :<C-u>call <sid>VisualAck()<cr>

