
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

" vim script for text filtering and alignment
Plug 'godlygeek/tabular'

" allow atom like line swapping with arrow keys
Plug 'madnight/vim-swap-lines'

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
" Plug 'w0rp/ale'

" comment out stuff via shortcut
Plug 'scrooloose/nerdcommenter'

" wrapper for fuzzy findec
Plug 'junegunn/fzf.vim'

" fuzzy file, buffer, mru, tag, etc finder with rg backend
Plug 'junegunn/fzf'

" shows a git diff in the 'gutter' (sign column)
Plug 'airblade/vim-gitgutter'

" fix gui only colorschemes to work in terminal
Plug 'godlygeek/csapprox'

" FocusGained and FocusLost autocommand events for tmxux
Plug 'tmux-plugins/vim-tmux-focus-events'

" A Vim plugin for more pleasant editing on commit messages
Plug 'rhysd/committia.vim'

" Interactive command execution in Vim.
Plug 'shougo/vimproc.vim', {'do' : 'make'}

" Changes Vim working directory to project root
Plug 'airblade/vim-rooter'

" Autoformat Haskell
Plug 'alx741/vim-hindent'

" Autoformat JS
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

" Highlight the exact differences, based on characters and words
Plug 'rickhowe/diffchar.vim'

" a collection of language packs for Vim.
Plug 'sheerun/vim-polyglot'

" Intellisense engine for vim8 & neovim, full language server protocol support
" Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}

" ################################
" # on command lazy loaded plugins
" ################################

" colorized filenames in nerdtree
Plug 'tiagofumo/vim-nerdtree-syntax-highlight', { 'on': 'NERDTreeToggle' }

" nerdtree git support
Plug 'Xuyuanp/nerdtree-git-plugin',             { 'on':  'NERDTreeToggle' }

" be able to write files with sudo right
Plug 'chrisbra/SudoEdit.vim',                   { 'on': 'SudoWrite' }

" jump between source and header (.cpp -> .h)
Plug 'vim-scripts/a.vim',                       { 'on': 'A'}

Plug 'whonore/Coqtail'

call plug#end()

