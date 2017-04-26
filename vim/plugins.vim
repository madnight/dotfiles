
 "              _             _
 "        _ __ | |_   _  __ _(_)_ __  ___
 "       | '_ \| | | | |/ _` | | '_ \/ __|
 "       | |_) | | |_| | (_| | | | | \__ \
 "       | .__/|_|\__,_|\__, |_|_| |_|___/
 "       |_|            |___/
 "

call plug#begin()
" always active plugins

" easymotion provides a much simpler way to use some motions in vim
Plug 'easymotion/vim-easymotion'
" editorconfig define consistent coding styles in different editors
Plug 'editorconfig/editorconfig-vim'
" Peekaboo extends " @ normal mode and <CTRL-R> insert mode see the contents of the registers
Plug 'junegunn/vim-peekaboo'
" incsearch.vim incrementally highlights ALL pattern matches
Plug 'haya14busa/incsearch.vim'
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
" a big collection of colorscheme
Plug 'flazz/vim-colorschemes'
" vim script for text filtering and alignment
Plug 'godlygeek/tabular'
" highlight variables differently
Plug 'jaxbot/semantic-highlight.vim'
" allow atom like line swapping with arrow keys
Plug 'madnight/vim-swap-lines'
" move selected text up and down
Plug 'matze/vim-move'
" global replace
Plug 'skwp/greplace.vim'
" ctags generator
Plug 'szw/vim-tags'
" select increasingly greater region of text
Plug 'terryma/vim-expand-region'
" vim sugar for the UNIX shell commands that need it the most
Plug 'tpope/vim-eunuch'
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
" easy tmux navigation
Plug 'christoomey/vim-tmux-navigator'
" vim status line themes
Plug 'vim-airline/vim-airline-themes'
" hybrid theme https://i.imgur.com/M3Qthm4.png
Plug 'w0ng/vim-hybrid'
" syntastic replacement that uses background threads for syntax check
Plug 'w0rp/ale'
" miscellaneous auto-load vim scripts
Plug 'xolox/vim-misc'
" comment out stuff via shortcut
Plug 'scrooloose/nerdcommenter'
" toggles between relative and absolute line numbers automatically
Plug 'jeffkreeftmeijer/vim-numbertoggle'


" plugins active on trigger (lazy loaded)

" a code-completion engine for Vim
Plug 'Valloric/YouCompleteMe',                  { 'on': [], 'do': function('BuildYCM') }
" resizer mode ctrl-e
Plug 'simeji/winresizer',                       { 'on': 'WinResizerStartResize' }
" hardtime helps you break that annoying habit vimmers have
Plug 'takac/vim-hardtime',                      { 'on': 'HardTimeOn' }
" Track the engine.
Plug 'SirVer/ultisnips',                        { 'on': [] }
" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets',                      { 'on': [] }
" an ack.vim alternative mimics Ctrl-Shift-F (search with context)
Plug 'dyng/ctrlsf.vim',                         { 'on': 'CtrlSF' }
" fuzzy file, buffer, mru, tag, etc finder with rg backend
Plug 'junegunn/fzf',                            { 'on': ['Files', 'History'], 'dir': '~/.fzf', 'do': './install --all' }
" wrapper for fuzzy finder
Plug 'junegunn/fzf.vim',                        { 'on': ['Files', 'History'] }
" autocomplete things that you see in other terminal
Plug 'wellle/tmux-complete.vim',                { 'on': [] }
" gitk for vim
Plug 'gregsexton/gitv',                         { 'on': 'Gitv' }
" improved nerdtree side panel (more ide like)
Plug 'jistr/vim-nerdtree-tabs',                 { 'on': 'NERDTreeToggle' }
" lightweight gitv (if gitv is too slow)
Plug 'junegunn/gv.vim',                         { 'on': 'GV' }
" debugger for php, python and other languages
Plug 'joonty/vdebug',                           { 'on': 'VdebugStart' }
" in file text string search of current dir with super fast rg
Plug 'mileszs/ack.vim',                         { 'on': 'Ack' }
" show mru (most recently used) files
Plug 'vim-scripts/mru.vim',                     { 'on': 'Mru' }
" delete all the buffers except the current buffer :Bonly
Plug 'schickling/vim-bufonly',                  { 'on': 'Bonly' }
" an ide like file explorer
Plug 'scrooloose/nerdtree',                     { 'on': 'NERDTreeToggle' }
" vim plugin to visualize your vim undo tree.
Plug 'sjl/gundo.vim',                           { 'on': 'Gundo' }
" colorized filenames in nerdtree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }
" jump between source and header (.cpp -> .h)
Plug 'vim-scripts/a.vim',                       { 'on': 'A'}
" nerdtree git support
Plug 'Xuyuanp/nerdtree-git-plugin',             { 'on':  'NERDTreeToggle' }
" distraction-free writing in Vim
Plug 'junegunn/goyo.vim',                       { 'on': 'Goyo' }
" search on google
Plug 'szw/vim-g',                               { 'on': 'Google' }
" cvim / vimium like window choosing by hinting
Plug 't9md/vim-choosewin',                      { 'on': 'ChooseWin' }
" be able to write files with sudo right
Plug 'chrisbra/SudoEdit.vim',                   { 'on': 'SudoWrite' }

" language specific plugins (lazy loaded)

" improved PHP omni-completion, based on the default phpcomplete.vim
Plug 'shawncplus/phpcomplete.vim',              { 'for': 'php' }
" markdown preview
Plug 'suan/vim-instant-markdown',               { 'for': 'markdown' }
" open markdown preview in browser
Plug 'JamshedVesuna/vim-markdown-preview',      { 'for': 'markdown' }
" syntax highlighting, matching rules and mappings for the original Markdown and extensions
Plug 'plasticboy/vim-markdown',                 { 'for': 'markdown' }
" HTML5 + inline SVG omnicomplete function, indent and syntax for vim
Plug 'othree/html5.vim',                        { 'for': 'html' }
" distinct highlighting of keywords vs values, JSON-specific warnings, quote concealing
Plug 'elzr/vim-json',                           { 'for': 'json' }
" adds some more stuff that I find useful, including all of my notes and customizations.
Plug 'derekwyatt/vim-scala',                    { 'for': 'scala' }
" css3 syntax highlight
Plug 'hail2u/vim-css3-syntax',                  { 'for': 'css' }
" java syntax highlight
Plug 'artur-shaik/vim-javacomplete2',           { 'for': 'java' }
" nginx syntax highlight
Plug 'evanmiller/nginx-vim-syntax',             { 'for': 'nginx' }
" coffeescript syntax support
Plug 'kchmck/vim-coffee-script',                { 'for': 'coffee' }
" colorize hex codes this plugin is still inefficient for large files
Plug 'gko/vim-coloresque',                      { 'for': 'css' }
" enhanced javascript synxtax highlighting
Plug 'jelera/vim-javascript-syntax',            { 'for': 'javascript' }
" vim support for react jsx
Plug 'mxw/vim-jsx',                             { 'for': 'javascript' }
" vim javascript support
Plug 'pangloss/vim-javascript',                 { 'for': 'javascript' }
" list of JavaScript ES6 snippets and syntax highlighting for vim
Plug 'isRuslan/vim-es6',                        { 'for': 'javascript' }
" wisely add end in ruby
Plug 'tpope/vim-endwise',                       { 'for': 'ruby' }
" enhanced cpp support
Plug 'octol/vim-cpp-enhanced-highlight',        { 'for': 'cpp' }
" code completion for python
Plug 'davidhalter/jedi-vim',                    { 'for': 'python' }
" vim syntax file for Docker's Dockerfile and snippets for snipMate
Plug 'ekalinin/Dockerfile.vim',                 { 'for': 'Dockerfile' }
" syntax highlighting for tmux
Plug 'tmux-plugins/vim-tmux',                   { 'for': 'tmux' }
" syntax highlighting, autocompletion for less
Plug 'groenewege/vim-less',                     { 'for': 'less' }
" runtime files for Haml, Sass, and SCSS that ship with vim
Plug 'tpope/vim-haml',                          { 'for' : ['haml','sass','scss','css'] }
" add go support for vim
Plug 'fatih/vim-go',                            { 'for': 'go' }
" offical rust plugin
Plug 'rust-lang/rust.vim',                      { 'for': 'rust' }


" disabled plugins

" automated tag file generation and syntax highlighting of tags in vim
" Plug 'xolox/vim-easytags' // disabled performance reasons

" nerdtree icons
" Plug 'ryanoasis/vim-devicons', { 'on':  'NERDTreeToggle' } // disabled performance reasons

" ide like tagbar that lists all function of a class / file
" Plug 'majutsushi/tagbar', {'on' : 'LdTagbar'} // disabled performance reasons

" shows a git diff in the 'gutter' (sign column) // disabled performance reasons
" Plug 'airblade/vim-gitgutter'

" Plug 'nixprime/cpsm' // does only work with vim python support

" generate fancy tmux status lines (airline like)
" Plug 'edkolev/tmuxline.vim',  {'on': ['Tmuxline']} // will be enabled if needed

call plug#end()


