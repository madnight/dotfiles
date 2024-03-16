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

-- git wrapper that should be illegal
Plug 'tpope/vim-fugitive'

-- vim tabline (top)
Plug 'akinsho/bufferline.nvim'

-- vim status line (bottom)
Plug 'nvim-lualine/lualine.nvim'

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

-- Autoformat JS use :Prettier command
Plug 'prettier/vim-prettier'

-- A collection of language packs for Vim.
Plug 'sheerun/vim-polyglot'

-- Indentation guides for Vim (Python)
Plug 'lukas-reineke/indent-blankline.nvim'

-- A Vim plugin for the Coq proof assistant, providing IDE-like features.
Plug 'dense-analysis/ale'

-- ChatGPT Integration
Plug 'MunifTanjim/nui.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'folke/trouble.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'jackmort/chatgpt.nvim'

-- TreeSitter Allows Highlighting + Commenting for multiple languages in one File
Plug 'nvim-treesitter/nvim-treesitter'

vim.call('plug#end')

require('lualine').setup {
  options = { theme  = 'modus-vivendi',  icons_enabled = false, },
}

vim.opt.termguicolors = true
local bufferline = require('bufferline')
    bufferline.setup {
        options = {
            indicator = {
                style = 'none',
            },
            buffer_close_icon = 'x',
            modified_icon = 'm',
            close_icon = 'x',
            left_trunc_marker = '<',
            right_trunc_marker = '>',
            offsets = {
                {
                    filetype = "NvimTree",
                    text = "File Tree",
                    text_align = "center",
                    separator = true
                }
            },
            show_buffer_icons = false,
            show_buffer_close_icons =  false,
            show_close_icon =  false,
            show_tab_indicators =  false,
            always_show_bufferline =  false,
            show_duplicate_prefix =  false,
        }
  }

vim.g.loaded_netrw = 0
vim.g.loaded_netrwPlugin = 0
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
-- Auto close nvim-tree when it's the last window
vim.api.nvim_create_autocmd("BufEnter", {
  nested = true,
  callback = function()
    if #vim.api.nvim_list_wins() == 1 and require("nvim-tree.utils").is_nvim_tree_buf() then
      vim.cmd "quit"
    end
  end
})

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

--  ##################
--  Basic vim settings
-- "##################
vim.o.background = 'dark'
vim.o.backspace = 'indent,eol,start'
vim.o.clipboard = 'unnamedplus'
vim.o.cmdheight = 1
vim.o.colorcolumn = '80'
vim.o.complete = '.,w,b,u,t'
vim.o.confirm = true
vim.o.cursorline = true
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
vim.o.visualbell = false
vim.o.errorbells = false
vim.o.shell = '/bin/bash'
vim.o.showmatch = true
vim.o.showmode = true
vim.o.smartcase = true
vim.o.title = true
vim.o.cmdheight = 0
vim.o.list = true
vim.o.listchars = 'tab:»-'
vim.o.tabstop = 4
vim.wo.number = true
vim.wo.relativenumber = true
vim.wo.list = true
vim.wo.listchars = 'tab:»-,trail:·,extends:»,precedes:«'
vim.wo.cursorline = false
vim.wo.wrap = true
vim.wo.scrolloff = 2
vim.wo.numberwidth = 1
vim.bo.fileformat = 'unix'
vim.bo.formatoptions = 'l'
vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.smartindent = true
vim.bo.softtabstop = 4
vim.bo.matchpairs = vim.bo.matchpairs .. ',<:>'
vim.g.loaded_matchparen = false
vim.api.nvim_set_keymap('n', '<F11>', '<PasteToggle>', { noremap = true })
vim.api.nvim_set_keymap('n', '<F11>', '<paste>', { noremap = true })
-- Set mapleader to comma
vim.g.mapleader = ","
vim.cmd('set diffopt+=context:99999') -- Appending to a Vim option
vim.cmd("filetype plugin on")
-- Enable syntax highlighting
vim.cmd("syntax on")
vim.cmd("set incsearch")
-- Disable compatibility mode
if vim.o.compatible then
    vim.o.nocompatible = true
end

vim.cmd('highlight OverLength ctermbg=black ctermfg=red')
vim.cmd('match OverLength /\\%81v.\\+/')

vim.api.nvim_set_keymap('n', '<Leader>n', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<Leader>nf', ':NvimTreeFindFile<CR>', { noremap = true, silent = true })

-- fix some typos
vim.cmd('colorscheme hybrid')
vim.cmd('syntax sync minlines=200')
vim.cmd('syntax on')
vim.cmd('filetype plugin indent on')
vim.cmd('hi clear CursorLine')
vim.cmd('hi clear SpellBad')
vim.cmd('autocmd VimEnter * nnoremap <Leader>cn :cnext<CR>')
vim.cmd('highlight LineNr ctermfg=DarkGrey')
vim.cmd('hi clear CursorLine')
vim.cmd('hi clear SpellBad')
vim.cmd('hi SpellBad cterm=underline ctermfg=red')
vim.cmd('hi LineNr guibg=#1D1F21')

-- Lua equivalent for Vim's autocmd
vim.api.nvim_create_augroup('plugin_initialize', {})

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


vim.keymap.set({ "n", "o", "x" }, "w", "<cmd>lua require('spider').motion('w')<CR>", { desc = "Spider-w" })
vim.keymap.set({ "n", "o", "x" }, "e", "<cmd>lua require('spider').motion('e')<CR>", { desc = "Spider-e" })
vim.keymap.set({ "n", "o", "x" }, "b", "<cmd>lua require('spider').motion('b')<CR>", { desc = "Spider-b" })
vim.keymap.set('n', '<Leader>n', function() vim.cmd(':NvimTreeToggle') end, { noremap = true })

vim.api.nvim_set_keymap('i', 'jj', '<Esc>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-s>', '<ESC>:w<CR>', {noremap = true})
vim.api.nvim_set_keymap('n', '<Leader>cp', ':cprev<CR>', {})
vim.api.nvim_set_keymap('n', '<ESC>n', ':cnext<CR>', {})
vim.api.nvim_set_keymap('n', '<ESC>p', ':cprev<CR>', {})
vim.api.nvim_set_keymap('n', '<leader><Up>', 'ddkP', {})
vim.api.nvim_set_keymap('n', '<leader><Down>', 'ddp', {})
vim.api.nvim_set_keymap('n', '<C-p>', ':Files<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>lb', ':e#<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>rg', ':Rg<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>vs', ':vsplit<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>hs', ':split<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>l', ':b#<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>q', ':q<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>wq', ':wq<cr>', {})
vim.api.nvim_set_keymap('n', '<Leader>bd', ':bd<cr>', {})
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', {})
vim.api.nvim_set_keymap('n', '<C-L>', ':nohl<CR><C-L>', {})
vim.api.nvim_set_keymap('n', ',i', 'i_<Esc>r', {})
vim.api.nvim_set_keymap('n', '<C-X>', ':bd<CR>', {})
vim.api.nvim_set_keymap('n', '<C-l>', ':bnext<CR>', {})
vim.api.nvim_set_keymap('n', '<C-h>', ':bprevious<CR>', {})
vim.api.nvim_set_keymap('n', '<Leader>gb', ':Git blame<cr>', {})

vim.api.nvim_create_user_command('WQ', 'wq', {})
vim.api.nvim_create_user_command('Wq', 'wq', {})
vim.api.nvim_create_user_command('Wqa', 'wqa', {})
vim.api.nvim_create_user_command('W', 'w', {})
vim.api.nvim_create_user_command('Q', 'q', {})


-- TreeSitter Allows Highlighting + Commenting for multiple languages in one File
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "lua", "vim" , "sql", "python", "svelte", "terraform", "javascript", "html", "css" },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}


vim.cmd([[
vnoremap <Leader>e :ChatGPTEditWithInstructions<cr>

" show trailing whitespace
highlight ExtraWhitespace ctermbg=darkred guibg=darkred
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()


" --------------------
" Plugin Configuration
" --------------------
let g:tmux_resizer_no_mappings = 1
" Async Lint Engine settings
let g:ale_lint_on_text_changed = 0
let g:ale_sign_error = '>>'
let g:ale_sign_column_always = 0
let g:ale_sign_warning = '--'
let g:ale_echo_msg_error_str = 'E'
let g:ale_echo_msg_warning_str = 'W'
let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
" Write this in your vimrc file
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 0
let g:ale_python_flake8_options = '--max-line-length=120'
" You can disable this option too
" if you don't want linters to run on opening a file
let g:ale_lint_on_enter = 0
let g:vim_json_syntax_conceal = 0
let g:prettier#quickfix_enabled = 0
" Silent Rooter otherwise it will echo the directory change
let g:rooter_silent_chdir = 1
" custom tmux navigator key maps
let g:tmux_navigator_no_mappings = 1
let g:vim_tags_auto_generate = 1
let g:prettier#autoformat = 0
let g:prettier#config#tab_width = 4
let g:prettier#config#use_tabs = 'false'
" lilydjwg/colorizer is inefficient for large files
let g:colorizer_maxlines = 200
let g:tmux_navigator_no_mappings = 1

" -------
" Keymaps
" -------
nmap <C-f> :Rg<cr>
nmap <S-k> <C-w>wl<CR>
nmap <S-w> :bd<CR>
" Jump faster
nmap <C-j> 4j
nmap <C-k> 4k
nmap <M-l> <C-w>l
nmap o o<ESC>
nmap ^ $
nmap zz ZZ
nnoremap <C-Down> :m .+1<CR>==
nnoremap <C-Up> :m .-2<CR>==
map <C-PageDown> <C-w>wl<CR>
imap jj <Esc><Esc>
map <C-PageUp> <C-w>wh<CR>
map <D-/> <C-_><C-_>
map Y y$
map <S-w> <ESC>:q!<CR>
map <F2> ]s
map <F4> z=
map <F5> :setlocal spell! spelllang=de_de,en_us<CR>
map <C-s> <ESC>:w<CR>
cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
inoremap jk <ESC>
" Ctrl+Delete to delete a word
inoremap <C-?> <C-W>
imap <C-H> <C-W>
silent inoremap <silent> <Tab> <C-n>
silent inoremap <silent> <S-Tab> <C-p>
vmap ^ $

silent! iunmap (
silent! iunmap )
silent! iunmap {
silent! iunmap }

" ---------------
" Autocmd Config
" ---------------
augroup vimrc_autocmd
  autocmd!
  au BufNewFile,BufRead,BufEnter *.{tex,txt} setlocal spell spelllang=de_de,en_gb
  au BufNewFile,BufRead,BufEnter *.yml set syntax=yaml
  au BufNewFile,BufRead *.conf set ft=apache

  au FileType javascript setlocal expandtab shiftwidth=4 tabstop=4
  au Filetype *.js setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4
  au Filetype *.jsx setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4

  set shiftwidth=4
  set expandtab

  " auto change path to current file (most compatible behaviour)
  au BufEnter * silent! lcd %:p:h
  au FileType mardown set spell spelllang=en_us

  " Adding automatons for when entering or leaving Vim
  if len(argv()) < 1
    au VimEnter * nested :call LoadSession()
    au VimLeave * :call MakeSession()
  endif

function! Profile()
  :profile start profile.log
  :profile func *
  :profile file *
endfunction

function! StopProfile()
  :profile pause
  :noautocmd qall!
endfunction

augroup END

fun! QuitPrompt()
      let choice = confirm("Close?", "&yes\n&no", 1)
      if choice == 1 | wq | endif
endfun
]])
-- End of old vimrc

-- Return cursor to where it was last time closing the file
vim.api.nvim_create_autocmd({'BufWinEnter'}, {
  pattern = '*',
  command = 'silent! normal! g`"zv',
})
