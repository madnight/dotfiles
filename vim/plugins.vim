
 "              _             _
 "        _ __ | |_   _  __ _(_)_ __  ___
 "       | '_ \| | | | |/ _` | | '_ \/ __|
 "       | |_) | | |_| | (_| | | | | \__ \
 "       | .__/|_|\__,_|\__, |_|_| |_|___/
 "       |_|            |___/
 "

call plug#begin()

" #####################
" always active plugins
" #####################

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

" add useful extra commands
Plug 'tpope/vim-unimpaired'

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

" Easy text exchange operator for Vim.
Plug 'twuommcdo/vim-exchange'

" hardtime helps you break that annoying habit vimmers have
Plug 'takac/vim-hardtime'

" wrapper for fuzzy findec
Plug 'junegunn/fzf.vim'

" fzf with mru
Plug 'tweekmonster/fzf-filemru'

" autoclose brackets
Plug 'townk/vim-autoclose'

" preview colours in source code while editing 
Plug 'ap/vim-css-color'

" shows a git diff in the 'gutter' (sign column) 
Plug 'airblade/vim-gitgutter'

" #######################################
" plugins active on trigger (lazy loaded)
" #######################################

" vim plugin to interact with tmux
Plug 'benmills/vimux',                          { 'on': 'VimuxRunCommand' }

" a code-completion engine for Vim
Plug 'Valloric/YouCompleteMe',                  { 'on': [], 'do': function('BuildYCM') }

" Track the engine.
Plug 'SirVer/ultisnips',                        { 'on': [] }

" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets',                      { 'on': [] }

" an ack.vim alternative mimics Ctrl-Shift-F (search with context)
Plug 'dyng/ctrlsf.vim',                         { 'on': 'CtrlSF' }

" fuzzy file, buffer, mru, tag, etc finder with rg backend
Plug 'junegunn/fzf',                            { 'on': ['FilesMru', 'Files', 'History'], 'dir': '~/.fzf', 'do': './install --all' }

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



" #########################################
" language specific plugins (lazy loaded)
" #########################################

" A collection of language packs for Vim.
Plug 'sheerun/vim-polyglot'

" ################
" disabled plugins
" ################

" automated tag file generation and syntax highlighting of tags in vim
" Plug 'xolox/vim-easytags' // disabled performance reasons

" nerdtree icons
" Plug 'ryanoasis/vim-devicons', { 'on':  'NERDTreeToggle' } // disabled performance reasons

" ide like tagbar that lists all function of a class / file
" Plug 'majutsushi/tagbar', {'on' : 'LdTagbar'} // disabled performance reasons

" Plug 'nixprime/cpsm' // does only work with vim python support

" generate fancy tmux status lines (airline like)
" Plug 'edkolev/tmuxline.vim',  {'on': ['Tmuxline']} // will be enabled if needed


" dont need this plugin anymore, due to tmux like window resizing
" resizer mode ctrl-e
" Plug 'simeji/winresizer',                       { 'on': 'WinResizerStartResize' }

call plug#end()


