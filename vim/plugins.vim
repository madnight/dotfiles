


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

" an ide like file explorer
Plug 'scrooloose/nerdtree'

" editorconfig define consistent coding styles in different editors
Plug 'editorconfig/editorconfig-vim'

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

" add useful extra commands for search and replace such as case sensitive rep
Plug 'tpope/vim-abolish'

" vim status line
Plug 'vim-airline/vim-airline'

" easy tmux navigation
Plug 'christoomey/vim-tmux-navigator'

" vim tmux resize integration
Plug 'madnight/vim-tmux-resizer'

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

" Easy text exchange operator for Vim.
Plug 'twuommcdo/vim-exchange'

" wrapper for fuzzy findec
Plug 'junegunn/fzf.vim'

" fuzzy file, buffer, mru, tag, etc finder with rg backend
Plug 'junegunn/fzf'

" fzf with mru
Plug 'tweekmonster/fzf-filemru'

" autoclose brackets
Plug 'townk/vim-autoclose'

" shows a git diff in the 'gutter' (sign column)
Plug 'airblade/vim-gitgutter'

" this plugin defines a new text object, based on indentation levels
Plug 'michaeljsmith/vim-indent-object'

" fix gui only colorschemes to work in terminal
Plug 'godlygeek/csapprox'

" easy text exchange operator for Vim
Plug 'tommcdo/vim-exchange'

" easymotion provides a much simpler way to use some motions in vim
Plug 'easymotion/vim-easymotion'

" FocusGained and FocusLost autocommand events for tmxux
Plug 'tmux-plugins/vim-tmux-focus-events'

" vim plugin that provides additional text objects
Plug 'wellle/targets.vim'

" A Vim plugin for more pleasant editing on commit messages
Plug 'rhysd/committia.vim'

" Interactive command execution in Vim.
Plug 'shougo/vimproc.vim', {'do' : 'make'}

" Rename the current file in the vim buffer + retain relative path.
Plug 'danro/rename.vim'

" Changes Vim working directory to project root
Plug 'airblade/vim-rooter'

" Autoformat Haskell
Plug 'alx741/vim-hindent'

" Highlight the exact differences, based on characters and words
Plug 'rickhowe/diffchar.vim'

" Intellisense engine for vim8 & neovim, full language server protocol support
Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}

" colorized filenames in nerdtree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }

" jump between source and header (.cpp -> .h)
Plug 'vim-scripts/a.vim',                       { 'on': 'A'}

" nerdtree git support
Plug 'Xuyuanp/nerdtree-git-plugin',             { 'on':  'NERDTreeToggle' }

" be able to write files with sudo right
Plug 'chrisbra/SudoEdit.vim',                   { 'on': 'SudoWrite' }


" colorized filenames in nerdtree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }

" jump between source and header (.cpp -> .h)
Plug 'vim-scripts/a.vim',                       { 'on': 'A'}

" nerdtree git support
Plug 'Xuyuanp/nerdtree-git-plugin',             { 'on':  'NERDTreeToggle' }

" be able to write files with sudo right
Plug 'chrisbra/SudoEdit.vim',                   { 'on': 'SudoWrite' }


" Plug 'edkolev/tmuxline.vim',                   { 'on': 'Tmuxline' }

" #########################################
" language specific plugins (lazy loaded)
" #########################################

" a collection of language packs for Vim.
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

call plug#end()

" Plug 'edkolev/tmuxline.vim',                   { 'on': 'Tmuxline' }

" #########################################
" language specific plugins (lazy loaded)
" #########################################

" a collection of language packs for Vim.
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


" #######################################
" plugins active on trigger (lazy loaded)
" #######################################


" function! BuildYCM(info)
"   if a:info.status == 'installed' || a:info.force
"     !./install.py
"   endif
" endfunction


" vim plugin to interact with tmux
Plug 'benmills/vimux',                          { 'on': 'VimuxRunCommand' }

" keep an eye in youcompleteme seems that
" the recompile on update might not work
" and the plugin after that neither

" a code-completion engine for Vim
" Plug 'Valloric/YouCompleteMe',                  { 'on': [], 'do': function('BuildYCM') }

" debugger for php, python and other languages
Plug 'joonty/vdebug',                           { 'on': 'VdebugStart' }

" delete all the buffers except the current buffer :Bonly
Plug 'schickling/vim-bufonly',                  { 'on': 'Bonly' }


" vim plugin to visualize your vim undo tree.
Plug 'sjl/gundo.vim',                           { 'on': 'Gundo' }

call plug#end()
