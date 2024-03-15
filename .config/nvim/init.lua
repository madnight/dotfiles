local Plug = vim.fn['plug#']

vim.call('plug#begin')

-- A color scheme for Vim that combines features from other themes.
Plug 'w0ng/vim-hybrid'

-- A plugin for seamless navigation between Neovim splits and tmux panes.
Plug 'alexghergh/nvim-tmux-navigation'

-- an ide like file explorer
Plug 'nvim-tree/nvim-tree.lua'

-- editorconfig define consistent coding styles in different editors
Plug 'editorconfig/editorconfig-vim'

-- camel case jumps + subword + skip insignifcant with w
Plug 'chrisgrieser/nvim-spider'

-- allow atom like line swapping with arrow keys
Plug 'madnight/vim-swap-lines'

-- git wrapper that should be illegal
Plug 'tpope/vim-fugitive'

-- vim status line
Plug 'vim-airline/vim-airline'

-- vim status line themes
Plug 'vim-airline/vim-airline-themes'

-- comment out stuff via shortcut
Plug 'preservim/nerdcommenter'

-- wrapper for fuzzy findec
Plug 'junegunn/fzf.vim'

-- fuzzy file, buffer, mru, tag, etc finder with rg backend
Plug 'junegunn/fzf'

-- A Vim plugin for more pleasant editing on commit messages
Plug 'rhysd/committia.vim'

-- Changes Vim working directory to project root
Plug 'airblade/vim-rooter'

-- Autoformat JS
Plug 'prettier/vim-prettier'

-- A collection of language packs for Vim.
Plug 'sheerun/vim-polyglot'

-- Indentation guides for Vim (Python)
Plug 'lukas-reineke/indent-blankline.nvim'


-- ChatGPT Integration
Plug 'MunifTanjim/nui.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'folke/trouble.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'jackmort/chatgpt.nvim'


-- ###################################################
-- Curresntly disabled, might be removed in the future
-- ###################################################

-- An implementation of the Popup API for Neovim in Lua.
-- Plug 'nvim-lua/popup.nvim'

-- A highly extendable fuzzy finder over lists for Neovim.
-- Plug 'nvim-telescope/telescope.nvim'

--- Can support all sort of whitespace characters e.g. zero-width spaces, non-breaking spaces, etc.
-- Plug 'ntpeters/vim-better-whitespace'

-- A Lua module that provides utility functions for Neovim plugins.
-- Plug 'nvim-lua/plenary.nvim'

-- A UI component library for Neovim to create user interfaces.
-- Plug 'MunifTanjim/nui.nvim'

-- provide easy code formatting in Vim by integrating existing code formatters
-- Plug 'Chiel92/vim-autoformat'

-- enhanced vim diff
-- Plug 'chrisbra/vim-diff-enhanced'

-- vim script for text filtering and alignment
-- Plug 'godlygeek/tabular'

-- minimal common sense vim tweaks
-- Plug 'tpope/vim-sensible'

-- vim sugar for the UNIX shell commands that need it the most
-- Plug 'tpope/vim-eunuch'

-- automatically adjusts 'shiftwidth' and 'expandtab' heuristically based
-- Plug 'tpope/vim-sleuth'

-- add parentheses arround current word or sentence
--Plug 'tpope/vim-surround'

-- add useful extra commands
-- Plug 'tpope/vim-unimpaired'

-- Interactive command execution in Vim.
-- Plug 'shougo/vimproc.vim'

-- shows a git diff in the 'gutter' (sign column)
-- Plug 'airblade/vim-gitgutter'

-- fix gui only colorschemes to work in terminal
-- Plug 'godlygeek/csapprox'

-- FocusGained and FocusLost autocommand events for tmxux
-- Plug 'tmux-plugins/vim-tmux-focus-events'

-- Highlight the exact differences, based on characters and words
--Plug 'rickhowe/diffchar.vim'

-- A Vim plugin for the Coq proof assistant, providing IDE-like features.
--Plug 'whonore/Coqtail'

vim.call('plug#end')


require("chatgpt").setup({
        actions_paths = { "~/.config/nvim/helpers/actions.json" },
        open_ai_params = {
          model = "gpt-4-0125-preview",
          temperature = 0,
        },
        openai_edit_params = {
          model = "gpt-4-0125-preview",
          temperature = 0,
        },
})

-- Enable indent-blankline only for Python files
require("ibl").setup { enabled = false }
vim.api.nvim_create_autocmd("FileType", {
    pattern = "python",
    callback = function()
        require("ibl").setup_buffer(0, { indent = { highlight = highlight, char = "|" }, enabled = true  })
    end,
})


-- ###################
--  Basic vim settings
-- "###################
vim.o.background = 'dark'
vim.o.backspace = 'indent,eol,start'
vim.o.colorcolumn = '80'
vim.o.clipboard = 'unnamedplus'
vim.o.cmdheight = 1
vim.o.complete = '.,w,b,u,t'
vim.o.confirm = true
vim.o.cursorline = true
vim.cmd('set diffopt+=context:99999') -- Appending to a Vim option
vim.o.encoding = 'utf-8'
vim.o.expandtab = true
vim.o.grepprg = 'rg --color never --line-number --no-heading'
vim.o.hidden = true
vim.o.history = 50
vim.o.ignorecase = true
vim.o.invnumber = true
vim.o.laststatus = 2
vim.o.linebreak = true
vim.o.magic = true
vim.o.matchtime = 2
vim.o.mouse = 'a'
vim.o.mousemodel = 'popup'
vim.o.nobackup = true
vim.o.mouse = 'v'
vim.o.nocompatible = true
vim.o.nocursorcolumn = true
vim.o.nocursorline = true
vim.o.nofoldenable = true
vim.o.nolazyredraw = true
vim.o.noruler = true
vim.o.noshowcmd = true
vim.o.nostartofline = true
vim.o.noswapfile = true
vim.o.notimeout = true
vim.o.timeoutlen = 1000
vim.o.ttimeoutlen = 0
vim.o.nowrap = true
vim.o.number = true
vim.o.ruler = true
vim.o.updatetime = 1000
vim.o.viminfo = '\'20,\"500'
vim.o.wildignore = '*.o,*.obj,*.bak,*.exe,*.py[co],*.swp,*~,*.pyc,.svn,*/.git/*,*/tmp/*,*.swp,*.so,*.zip,*/node_modules'
vim.o.wildmenu = true
vim.o.wildmode = 'longest:list,full'
vim.o.wrap = true
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.list = true
vim.wo.listchars = 'tab:»-,trail:·,extends:»,precedes:«'
vim.wo.cursorline = false
vim.wo.wrap = true
vim.bo.fileformat = 'unix'
vim.bo.formatoptions = 'l'
vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.smartindent = true
vim.bo.softtabstop = 4
vim.bo.matchpairs = vim.bo.matchpairs .. ',<:>'
vim.api.nvim_set_keymap('n', '<F11>', '<PasteToggle>', { noremap = true })
vim.o.visualbell = false
vim.o.errorbells = false
vim.wo.scrolloff = 2
vim.o.shell = '/bin/bash'
vim.wo.numberwidth = 1
vim.api.nvim_set_keymap('n', '<F11>', '<paste>', { noremap = true })
vim.o.showmatch = true
vim.o.showmode = true
vim.o.smartcase = true
vim.o.title = true

vim.cmd("filetype plugin on")

-- Enable syntax highlighting
vim.cmd("syntax on")

vim.cmd("set incsearch")

-- Set mapleader to comma
vim.g.mapleader = ","

-- Disable compatibility mode
if vim.o.compatible then
    vim.o.nocompatible = true
end

-- set cmdheight=0
vim.o.cmdheight = 0

-- hi OverLength ctermbg=black ctermfg=red
-- match OverLength /\%81v.\+/
vim.cmd('highlight OverLength ctermbg=black ctermfg=red')
vim.cmd('match OverLength /\\%81v.\\+/')

-- fix some typos
vim.api.nvim_create_user_command('WQ', 'wq', {})
vim.api.nvim_create_user_command('Wq', 'wq', {})
vim.api.nvim_create_user_command('Wqa', 'wqa', {})
vim.api.nvim_create_user_command('W', 'w', {})
vim.api.nvim_create_user_command('Q', 'q', {})

vim.cmd('colorscheme hybrid')

-- syntax sync minlines=200
vim.cmd('syntax sync minlines=200')

-- syntax on
-- filetype plugin indent on
vim.cmd('syntax on')
vim.cmd('filetype plugin indent on')

vim.cmd('hi clear CursorLine')
vim.cmd('hi clear SpellBad')
-- set relativenumber
vim.wo.relativenumber = true

-- set list
-- set listchars=tab:»-
vim.o.list = true
vim.o.listchars = 'tab:»-'

-- set tabstop=4
vim.o.tabstop = 4

-- imap jj <Esc>
-- map <C-s> <ESC>:w<CR>
vim.api.nvim_set_keymap('i', 'jj', '<Esc>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-s>', '<ESC>:w<CR>', {noremap = true})

-- Lua equivalent for Vim's autocmd
vim.api.nvim_create_augroup('plugin_initialize', {})
vim.api.nvim_create_autocmd('VimEnter', {
    pattern = '*',
    callback = function()
        if vim.fn.exists(':NoMatchParen') == 2 then  -- Verify that the command exists
            vim.cmd('NoMatchParen')
        end
    end,
    group = 'plugin_initialize',
})

require'nvim-tmux-navigation'.setup {
    disable_when_zoomed = true,
    keybindings = {
            left = "<ESC>l",
            down = "<ESC>j",
            up = "<ESC>k",
            right = "<ESC>h",
            last_active = "<C-\\>",
            next = "<ESC><Space>",
        }
}


vim.keymap.set(
	{ "n", "o", "x" },
	"w",
	"<cmd>lua require('spider').motion('w')<CR>",
	{ desc = "Spider-w" }
)
vim.keymap.set(
	{ "n", "o", "x" },
	"e",
	"<cmd>lua require('spider').motion('e')<CR>",
	{ desc = "Spider-e" }
)
vim.keymap.set(
	{ "n", "o", "x" },
	"b",
	"<cmd>lua require('spider').motion('b')<CR>",
	{ desc = "Spider-b" }
)


vim.cmd('autocmd VimEnter * nnoremap <Leader>cn :cnext<CR>')
vim.api.nvim_set_keymap('n', '<Leader>cp', ':cprev<CR>', {})
vim.api.nvim_set_keymap('n', '<ESC>n', ':cnext<CR>', {})
vim.api.nvim_set_keymap('n', '<ESC>p', ':cprev<CR>', {})

vim.cmd('highlight LineNr ctermfg=DarkGrey')
vim.cmd('hi clear CursorLine')
vim.cmd('hi clear SpellBad')
vim.cmd('hi SpellBad cterm=underline ctermfg=red')
vim.cmd('hi LineNr guibg=#1D1F21')

vim.cmd [[
vnoremap <Leader>e :ChatGPTEditWithInstructions<cr>

function! s:Highlight_Matching_Pair()
endfunction

function! s:FindMatchingPair()
endfunction

function! s:Find_Matching_Pair()
endfunction

" show trailing whitespace
highlight ExtraWhitespace ctermbg=darkred guibg=darkred
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

 "              _             _                              __
 "        _ __ | |_   _  __ _(_)_ __  ___    ___ ___  _ __  / _|
 "       | '_ \| | | | |/ _` | | '_ \/ __|  / __/ _ \| '_ \| |_
 "       | |_) | | |_| | (_| | | | | \__ \ | (_| (_) | | | |  _|
 "       | .__/|_|\__,_|\__, |_|_| |_|___/  \___\___/|_| |_|_|
 "       |_|            |___/
 "
 "
let g:tmux_resizer_no_mappings = 1

"################
" Latex settings
"################
let g:livepreview_previewer = 'evince'
let g:Tex_CompileRule_pdf = 'latexmk -pdf'
let g:LatexBox_latexmk_options = "-pvc -pdfps"
let g:LatexBox_latexmk_preview_continuously = 1
let g:LatexBox_latexmk_async = 1

" performance optimization
let g:vimtex_motion_matchparen = 0

"##################
" Airline settings
"##################
let g:airline_powerline_fonts = 1
let g:airline_theme='molokai'
let g:airline_skip_empty_sections = 1
let g:airline#extensions#tabline#buffer_min_count =2
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

" performance optimization
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#branch#enabled = 0
let g:airline#extensions#whitespace#enabled = 0

"####################
" Tmux Line settings
"####################
let g:tmuxline_theme = 'zenburn'

"############################
" Async Lint Engine settings
"############################
let g:ale_lint_on_text_changed = 0
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '--'

let g:vim_json_syntax_conceal = 0
let g:prettier#quickfix_enabled = 0

" custom tmux navigator key maps
let g:tmux_navigator_no_mappings = 1
let g:tmuxcomplete#trigger = 'omnifunc'
let g:vim_tags_auto_generate = 1

let g:prettier#autoformat = 0
let g:prettier#config#tab_width = 4
let g:prettier#config#use_tabs = 'false'

let g:formatprg_js = "js-beautify"
let g:formatprg_args_js = "-i %@"

" lilydjwg/colorizer is inefficient for large files
let g:colorizer_maxlines = 200

let php_sql_query = 1
let php_htmlInStrings = 1
let Tlist_Use_Right_Window = 1
let g:jsx_ext_required = 0

" overcome limit imposed by max height
let g:ackprg = 'rg --vimgrep'
set grepprg=rg\ --vimgrep
let g:tmux_navigator_no_mappings = 1

let g:winresizer_horiz_resize = 1
let g:vim_markdown_preview_github=1

let g:rg_command = '
  \ rg --column --line-number --no-heading --fixed-strings --ignore-case --no-ignore --hidden --follow --color "always"
  \ -g "*.{js,json,php,md,styl,jade,html,config,py,cpp,c,go,hs,rb,conf}"
  \ -g "!{.git,node_modules,vendor}/*" '

command! -bang -nargs=* F call fzf#vim#grep(g:rg_command .shellescape(<q-args>), 1, <bang>0)

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
   \   <bang>0)

" Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)


autocmd BufRead,BufNewFile ~/.xmonad/* call s:add_xmonad_path()
function! s:add_xmonad_path()
  if !exists('b:ghcmod_ghc_options')
    let b:ghcmod_ghc_options = []
  endif
  call add(b:ghcmod_ghc_options, '-i' . expand('~/.xmonad/XMonad'))
endfunction

let g:haskell_indent_disable=1

let g:ale_linters = {
\   'haskell': ['hlint', 'ghc-mod', 'hdevtools', 'stack_build', 'stack_ghc'],
\}


"         _
"        | | _____ _   _ ___
"        | |/ / _ \ | | / __|
"        |   <  __/ |_| \__ \
"        |_|\_\___|\__, |___/
"                  |___/


imap <TAB> <SPACE><SPACE><SPACE><SPACE>
nmap <leader>ch1 :set ch=1<CR>
nmap <leader>ch20 :set ch=20<CR>
nmap <leader>hit :GhcModTypeInsert<CR>
nmap <leader>htc :GhcModTypeClear<cr>
nmap <leader>ht :GhcModType<cr>
nmap <leader>r :NERDTreeFind<cr>
nmap <silent> I "=nr2char(getchar())<cr>P
"This allows for change paste motion cp{motion}
nmap <silent> cp :set opfunc=ChangePaste<CR>g@
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction

noremap <Leader>sen :set spell spelllang=en_us<cr>
noremap <Leader>gen :set spell spelllang=de_de<cr>

function LightTheme()
    :silent exec "! sed -i 's/hybrid/Tomorrow/g' ~/.vim-theme-swtich"
    :silent exec "! sed -i 's/=dark/=light/g' ~/.vim-theme-switch"
endfunction

function DarkTheme()
    :silent exec "! sed -i 's/Tomorrow/hybrid/g' ~/.vim-theme-switch"
    :silent exec "! sed -i 's/=light/=dark/g' ~/.vim-theme-switch"
endfunction

map s <Plug>(easymotion-overwin-f)
let g:EasyMotion_keys = 'abcdefghijklmnopqrstuvwxyz'

" write as sudo
" command W w !sudo tee % > /dev/null
" swap words
nmap <silent> gw "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><C-o>:noh<CR>
" push current line up or down
nnoremap <leader><Up> ddkP
nnoremap <leader><Down> ddp
 " push word under cursor to the left
nnoremap <C-Left> "_yiw?\w\+\_W\+\%#<CR>:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><C-o><C-l>
" push word under cursor to the right
nnoremap <C-Right> "_yiw:s/\(\%#\w\+\)\(\_W\+\)\(\w\+\)/\3\2\1/<CR><C-o>/\w\+\_W\+<CR><C-l>

" tpope mapping (ü = [ us keyboard)
noremap <Leader>lt :set background=light<cr>:colorscheme Tomorrow<cr>
nnoremap <Leader>dt :set background=dark<cr>:colorscheme hybrid<cr>
nnoremap <Leader>te :call DarkTheme()<cr>

" find other mappings J is in use
" map K <Plug>(expand_region_expand)
" map J <Plug>(expand_region_shrink)
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)

nnoremap <C-p> :Files<cr>
nnoremap <Leader>lb :e#<cr>

nnoremap <Leader>rg :Rg<cr>
" nnoremap <C-I> i <ESC>r

" o insert line below
" O insert line above
" noremap + [
"
" nnoremap <Leader>cn :cnext<cr>


nnoremap <Leader>fh :History<cr>
nnoremap <Leader>fb :Buffer<cr>
nnoremap <Leader>vs :vsplit<cr>
nnoremap <Leader>hs :split<cr>
nnoremap <Leader>l :b#<cr>
nnoremap <Leader>q :q<cr>
nnoremap <Leader>wq :wq<cr>
nnoremap <Leader>bd :bd<cr>
nnoremap <Leader>vl :VimuxInterruptRunner<cr>:VimuxRunLastCommand<cr>
nnoremap ´ :VimuxRunLastCommand<cr>
nnoremap <Leader>vk :VimuxInterruptRunner<cr>
nnoremap K :Ack! '<C-r><C-w>'<cr>
nnoremap <C-j> <C-w>j
nnoremap K :Ack! '<C-r><C-w>'<cr>
" serach Most recently used (MRU) files (native vim function [oldfiles])
" nnoremap <C-m> :History<CR>
nnoremap <leader>cd :cd<CR>
" nnoremap <SPACE> :<C-f>
nnoremap <silent> <Leader>n :NvimTreeToggle<CR>

nnoremap <C-L> :nohl<CR><C-L>
" remove trailing white spaces
nnoremap <Leader>rw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>dw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>fu :CtrlPFunky<Cr>
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
nnoremap <silent> <C-e> :WinResizerStartResize<CR>
" nnoremap <silent> <C-p> :Files<CR>
nnoremap ZZ :call QuitPrompt()<cr>
" Alt / Mod Key (A-,M-) := <ESC>


noremap <Leader>a :Ack <cword><cr>
" the space after ACK here is intentional
noremap <Leader>ack :Ack 
noremap <C-X> :bd<CR>
noremap <silent> <c-up> :call SwapUp()<CR>
noremap <silent> <c-down> :call SwapDown()<CR>
noremap <C-l> :bnext<CR>
noremap <C-h> :bprevious<CR>
noremap <Leader>rn :call NumberToggle()<CR>
" fugitive shortcuts
noremap <Leader>gs :Gstatus<cr>
noremap <Leader>gc :Gcommit<cr>
noremap <Leader>ga :Gwrite<cr>
noremap <Leader>gl :Glog<cr>
noremap <Leader>gh :Glog<cr>
noremap <Leader>gd :Gdiff<cr>
noremap <Leader>gb :Git blame<cr>

nmap <Leader>cw <ESC>
nmap <Leader>s <Plug>(easymotion-overwin-f)

nmap <C-f> :Rg<cr>

nmap <Leader>rv <ESC>:so ~/.vimrc<CR>
nmap <Leader>pi <ESC>:PlugInstall<CR>
nmap <Leader>v <ESC>:so ~/.vimrc<CR>
nmap <Leader>cw <ESC>:ChooseWin<CR>
nmap ya y$
nmap <S-k> <C-w>wl<CR>
nmap <S-w> :bd<CR>
" Jump faster
nmap <C-j> 4j
nmap <M-l> <C-w>l
" Jump to definition (ctags -R)
nmap <C-b> <C-]>
nmap o o<ESC>
nmap <C-k> 4k
nmap <C-a> :A<CR>
" nmap <C-o> O<Esc>
nmap ^ $
nmap zz ZZ
nmap <C-g> <Plug>IMAP_JumpForward

map <C-PageUp> <C-w>wh<CR>
map <C-PageDown> <C-w>wl<CR>
map <D-/> <C-_><C-_>
map Y y$
map <S-w> <ESC>:q!<CR>

map <F2> ]s
map <F4> z=
map <F5> :setlocal spell! spelllang=de_de,en_us<CR>

nnoremap ,i i_<Esc>r
imap jj <Esc><Esc>
map <C-s> <ESC>:w<CR>
map <C-@> <ESC>:w<CR>:VimuxInterruptRunner<cr>:VimuxRunLastCommand<cr>
imap <C-g> <Plug>IMAP_JumpForward

vnoremap K :<C-u>call <sid>VisualAck()<cr>
vnoremap K :<C-u>call <sid>VisualAck()<cr>

cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
cnoremap <silent> x<cr> call QuitPrompt()<cr>

inoremap jk <ESC>
" Ctrl+Delete to delete a word
"
inoremap <C-?> <C-W>

silent inoremap <silent> <Tab> <C-n>
silent inoremap <silent> <S-Tab> <C-p>

vmap ^ $

silent! iunmap (
silent! iunmap )
silent! iunmap {
silent! iunmap }


" AUTOCMD Config

" Jump to the last known cursor position when opening a file.
augroup vimrc
  au!
  au BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
augroup END

augroup numbertoggle
  au!
  au BufEnter,FocusGained,InsertLeave * set relativenumber
  au BufLeave,FocusLost,InsertEnter   * set norelativenumber
augroup END

augroup vimrc_autocmd
  autocmd!
  au BufNewFile,BufRead,BufEnter *.{tex,txt} setlocal spell spelllang=de_de,en_gb
  au BufNewFile,BufRead,BufEnter *.pl set filetype=prolog
  au BufNewFile,BufRead,BufEnter *.yml set syntax=yaml
  au BufRead,BufNewFile,BufEnter *mutt* set filetype=mail
  au BufNewFile,BufRead *.coffee set filetype=coffee
  au BufNewFile,BufRead *.conf set ft=apache

  au FileType javascript setlocal expandtab shiftwidth=4 tabstop=4
  au Filetype *.js setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4
  au Filetype *.jsx setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4

  filetype plugin indent on

  " show existing tab with 4 spaces width
  set tabstop=4

  " when indenting with '>', use 4 spaces width
  set shiftwidth=4

  " On pressing tab, insert 4 spaces
  set expandtab

  au BufEnter *.hs set formatprg=xargs\ -0\ pointfree

  " auto change path to current file (most compatible behaviour)
  au BufEnter * silent! lcd %:p:h
  au FileType mardown set spell spelllang=en_us

  au VimResized * wincmd =

  " Adding automatons for when entering or leaving Vim
  if len(argv()) < 1
    au VimEnter * nested :call LoadSession()
    au VimLeave * NERDTreeClose
    au VimLeave * MBEClose
    au VimLeave * :call MakeSession()
  endif

augroup END

autocmd BufRead,BufNewFile ~/.xmonad/* call s:add_xmonad_path()
function! s:add_xmonad_path()
  if !exists('b:ghcmod_ghc_options')
    let b:ghcmod_ghc_options = []
  endif
  call add(b:ghcmod_ghc_options, '-i' . expand('~/.xmonad/lib'))
endfunction

" ask to auto create directory and file if not exsistent on save
augroup vimrc-auto-mkdir
  autocmd!
  autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
  function! s:auto_mkdir(dir, force)
    if !isdirectory(a:dir)
          \   && (a:force
          \       || input("'" . a:dir . "' does not exist. Create? [y/N]") =~? '^y\%[es]$')
      call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
    endif
  endfunction
augroup END

" i want always be able to edit the files
:autocmd BufWinEnter * setlocal modifiable


function! Profile()
  :profile start profile.log
  :profile func *
  :profile file *
endfunction

function! StopProfile()
  :profile pause
  :noautocmd qall!
endfunction

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

fun! QuitPrompt()
      let choice = confirm("Close?", "&yes\n&no", 1)
      if choice == 1 | wq | endif
endfun
]]


-- ################
-- NvimTree Settings
-- ################

-- disable netrw at the very start of your init.lua
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- optionally enable 24-bit colour
vim.opt.termguicolors = true

require("nvim-tree").setup({
  sort = {
    sorter = "case_sensitive",
  },
  view = {
    width = 35,
  },
  renderer = {
    icons = {
      glyphs = {
        default = "",
        folder = {
          arrow_open = "V",
          arrow_closed = ">",
          default = "",
          open = "",
          symlink = "S",
          empty = "E",
          empty_open = "EO",
          symlink_open = "SO",
        },

      }

    }
  },
  filters = {
    dotfiles = true,
  },
})

vim.api.nvim_set_keymap('n', '<Leader>n', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>nf', ':NvimTreeFindFile<CR>', { noremap = true, silent = true })

-- close nvim-tree when it's the last window
vim.api.nvim_create_autocmd("BufEnter", {
  nested = true,
  callback = function()
    if #vim.api.nvim_list_wins() == 1 and require("nvim-tree.utils").is_nvim_tree_buf() then
      vim.cmd "quit"
    end
  end
})
