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

-- Colorize hex/rgb/hsl/css colors in vim, super fast
Plug 'NvChad/nvim-colorizer.lua'

-- ChatGPT Integration
Plug 'MunifTanjim/nui.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'folke/trouble.nvim'
Plug 'nvim-telescope/telescope.nvim'
Plug 'jackmort/chatgpt.nvim'

-- TreeSitter Allows Highlighting + Commenting for multiple languages in one File
Plug 'nvim-treesitter/nvim-treesitter'

vim.call('plug#end')

require 'colorizer'.setup()

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

-- TreeSitter Allows Highlighting + Commenting for multiple languages in one File
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "lua", "vim" , "sql", "python", "svelte", "terraform", "javascript", "html", "css" },
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false,
  },
}

-- Enable indent-blankline only for Python files
require("ibl").setup { enabled = false }
vim.api.nvim_create_autocmd("FileType", {
    pattern = "python",
    callback = function()
        require("ibl").setup_buffer(0, { indent = { highlight = highlight, char = "|" }, enabled = true,  scope = { enabled = false },  })
    end,
})

--  ##################
--  Basic vim settings
-- "##################
vim.o.background = 'dark'
vim.o.backspace = 'indent,eol,start'
vim.o.clipboard = 'unnamedplus'
vim.o.colorcolumn = '80'
vim.o.complete = '.,w,b,u,t'
vim.o.confirm = true
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
vim.o.mousemodel = 'popup'
vim.o.nobackup = true
vim.o.mouse = 'v'
vim.o.nocursorcolumn = true
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
vim.o.tabstop = 4
vim.wo.relativenumber = true
vim.wo.listchars = 'tab:»-,trail:·,extends:»,precedes:«'
vim.wo.scrolloff = 2
vim.wo.numberwidth = 1
vim.bo.fileformat = 'unix'
vim.bo.formatoptions = 'l'
vim.bo.shiftwidth = 4
vim.bo.tabstop = 4
vim.bo.smartindent = true
vim.bo.softtabstop = 4
vim.bo.matchpairs = vim.bo.matchpairs .. ',<:>'
vim.g.mapleader = ","
vim.g.loaded_matchparen = false

-- Set the colorscheme and syntax highlighting
vim.cmd('set diffopt+=context:99999')
vim.cmd("filetype plugin on")
vim.cmd("syntax on")
vim.cmd("set incsearch")
vim.cmd('highlight OverLength ctermbg=black ctermfg=red')
vim.cmd('match OverLength /\\%81v.\\+/')
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
vim.cmd('highlight ExtraWhitespace ctermbg=darkred guibg=darkred')

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


-- -------
-- Keymaps
-- -------
for _, key in ipairs{"w", "e", "b"} do
  vim.keymap.set({"n", "o", "x"}, key, string.format("<cmd>lua require('spider').motion('%s')<CR>", key))
end

vim.keymap.set('n', '<Leader>n', function() vim.cmd(':NvimTreeToggle') end, { noremap = true })
vim.keymap.set('i', 'jj', '<Esc>', {noremap = true})
vim.keymap.set('n', '<C-s>', '<ESC>:w<CR>', {noremap = true})
vim.keymap.set('n', '<Leader>cp', ':cprev<CR>', {})
vim.keymap.set('n', '<ESC>n', ':cnext<CR>', {})
vim.keymap.set('n', '<ESC>p', ':cprev<CR>', {})
vim.keymap.set('n', '<C-p>', ':Files<cr>', {})
vim.keymap.set('n', '<Leader>rg', ':Rg<cr>', {})
vim.keymap.set('n', '<Leader>vs', ':vsplit<cr>', {})
vim.keymap.set('n', '<Leader>hs', ':split<cr>', {})
vim.keymap.set('n', '<Leader>bd', ':bd<cr>', {})
vim.keymap.set('n', '<C-j>', '<C-w>j', {})
vim.keymap.set('n', '<C-L>', ':nohl<CR><C-L>', {})
vim.keymap.set('n', '<C-l>', ':bnext<CR>', {})
vim.keymap.set('n', '<C-h>', ':bprevious<CR>', {})
vim.keymap.set('n', '<Leader>gb', ':Git blame<cr>', {})
vim.keymap.set('n', '<Leader>n', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.keymap.set('n', '<Leader>nf', ':NvimTreeFindFile<CR>', { noremap = true, silent = true })
vim.keymap.set('n', '<Leader>q', ':q<cr>', {})
vim.keymap.set('n', '<Leader>wq', ':wq<cr>', {})
vim.keymap.set('n', '<Leader>e', ':ChatGPTEditWithInstructions<cr>', { noremap = true })
vim.keymap.set('n', '<C-f>', ':Rg<cr>', {})
vim.keymap.set('n', '<S-w>', ':bd<CR>', {})
vim.keymap.set('n', '<C-j>', '4j', {})
vim.keymap.set('n', '<C-k>', '4k', {})
vim.keymap.set('n', 'o', 'o<ESC>', {})
vim.keymap.set('n', '<C-Down>', ':m .+1<CR>==', { noremap = true })
vim.keymap.set('n', '<C-Up>', ':m .-2<CR>==', { noremap = true })
vim.keymap.set('i', 'jj', '<Esc><Esc>', {})
vim.keymap.set('', '<S-w>', '<ESC>:q!<CR>', {})
vim.keymap.set('', '<F5>', ':setlocal spell! spelllang=de_de,en_us<CR>', {})
vim.keymap.set('', '<C-s>', '<ESC>:w<CR>', {})
vim.keymap.set('c', '<Leader>q<cr>', 'call QuitPrompt()<cr>', { silent = true, noremap = true })
vim.keymap.set('c', '<Leader>wq<cr>', 'call QuitPrompt()<cr>', { silent = true, noremap = true })
vim.keymap.set('i', 'jk', '<ESC>', {})
vim.keymap.set('i', '<C-?>', '<C-W>', {})
vim.keymap.set('i', '<C-H>', '<C-W>', {})

vim.api.nvim_create_user_command('WQ', 'wq', {})
vim.api.nvim_create_user_command('Wq', 'wq', {})
vim.api.nvim_create_user_command('Wqa', 'wqa', {})
vim.api.nvim_create_user_command('W', 'w', {})
vim.api.nvim_create_user_command('Q', 'q', {})

-- --------------------
-- Plugin Configuration
-- --------------------
vim.g.tmux_resizer_no_mappings = 1
-- Async Lint Engine settings
vim.g.ale_lint_on_text_changed = 0
vim.g.ale_sign_error = '>>'
vim.g.ale_sign_column_always = 0
vim.g.ale_sign_warning = '--'
vim.g.ale_echo_msg_error_str = 'E'
vim.g.ale_echo_msg_warning_str = 'W'
vim.g.ale_echo_msg_format = '[%linter%] %s [%severity%]'
-- Write this in your vimrc file
vim.g.ale_lint_on_text_changed = 'never'
vim.g.ale_lint_on_insert_leave = 0
vim.g.ale_python_flake8_options = '--max-line-length=120'
-- if you don't want linters to run on opening a file
vim.g.ale_lint_on_enter = 0
vim.g.vim_json_syntax_conceal = 0
vim.g.prettier_quickfix_enabled = 0
-- Silent Rooter otherwise it will echo the directory change
vim.g.rooter_silent_chdir = 1
-- custom tmux navigator key maps
vim.g.tmux_navigator_no_mappings = 1
vim.g.vim_tags_auto_generate = 1
vim.g.prettier_autoformat = 0
vim.g.prettier_config_tab_width = 4
vim.g.prettier_config_use_tabs = 'false'
-- lilydjwg/colorizer is inefficient for large files
vim.g.colorizer_maxlines = 200
vim.g.tmux_navigator_no_mappings = 1

-- ----------------
-- Helper Functions
-- ----------------
function Profile()
  vim.cmd('profile start profile.log')
  vim.cmd('profile func *')
  vim.cmd('profile file *')
end

function StopProfile()
  vim.cmd('profile pause')
  vim.cmd('noautocmd qall!')
end

local function QuitPrompt()
  if vim.fn.confirm("Close?", "&yes\n&no", 1) == 1 then
    vim.cmd('wq')
  end
end

-- ---------
-- Autocmds
-- ---------
vim.api.nvim_create_autocmd("BufWinEnter", {
  pattern = "*",
  command = "match ExtraWhitespace /\\s\\+$/"
})

vim.api.nvim_create_autocmd("InsertEnter", {
  pattern = "*",
  command = "match ExtraWhitespace /\\s\\+\\%#\\@<!$/"
})

vim.api.nvim_create_autocmd("InsertLeave", {
  pattern = "*",
  command = "match ExtraWhitespace /\\s\\+$/"
})

vim.api.nvim_create_autocmd("BufWinLeave", {
  pattern = "*",
  command = "call clearmatches()"
})

-- Return cursor to where it was last time closing the file
vim.api.nvim_create_autocmd({'BufWinEnter'}, {
  pattern = '*',
  command = 'silent! normal! g`"zv',
})
