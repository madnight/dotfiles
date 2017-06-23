
"         _
"        | | _____ _   _ ___
"        | |/ / _ \ | | / __|
"        |   <  __/ |_| \__ \
"        |_|\_\___|\__, |___/
"                  |___/


" unmap <Enter>
"
nmap <leader>nf :NERDTreeFind<cr>
nmap <silent> I "=nr2char(getchar())<cr>P
"This allows for change paste motion cp{motion}
nmap <silent> cp :set opfunc=ChangePaste<CR>g@
function! ChangePaste(type, ...)
    silent exe "normal! `[v`]\"_c"
    silent exe "normal! p"
endfunction

noremap <Leader>sen :set spell spelllang=en_us<cr>

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
command W w !sudo tee % > /dev/null
map / <Plug>(incsearch-forward)
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

nnoremap <C-p> :FilesMru --tiebreak=end<cr>
nnoremap <Leader>lb :e#<cr>
" nnoremap <C-I> i <ESC>r

" o insert line below
" O insert line above
" noremap + [
"
nnoremap <Leader>cn :cnext<cr>
nnoremap <Leader>cp :cprev<cr>
nnoremap <ESC>n :cnext<cr>
nnoremap <ESC>p :cprev<cr>
nnoremap + <C-a>
nnoremap - <C-x>
nnoremap ä <C-a>
nnoremap <Leader>fh :History<cr>
nnoremap <Leader>fb :Buffer<cr>
nnoremap <Leader>vs :vsplit<cr>
nnoremap <Leader>hs :split<cr>
nnoremap <Leader>l :b#<cr>
nnoremap <Leader>q :q<cr>
nnoremap <Leader>wq :wq<cr>
nnoremap <Leader>bd :bd<cr>
nnoremap <Leader>vl :VimuxInterruptRunner<cr>:VimuxRunLastCommand<cr>
nnoremap <Leader>vk :VimuxInterruptRunner<cr>
nnoremap K :Ack! '<C-r><C-w>'<cr>
nnoremap <C-j> <C-w>j
nnoremap K :Ack! '<C-r><C-w>'<cr>
" serach Most recently used (MRU) files (native vim function [oldfiles])
" nnoremap <C-m> :History<CR>
nnoremap <Leader>cd :lcd %:p:h<CR>
" nnoremap <SPACE> :<C-f>
nnoremap <Leader>n :NERDTreeToggle<CR>
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

nnoremap <silent> <ESC>h :TmuxNavigateLeft<cr>
nnoremap <silent> <ESC>l :TmuxNavigateRight<cr>
nnoremap <silent> <ESC>k :TmuxNavigateDown<cr>
nnoremap <silent> <ESC>j :TmuxNavigateUp<cr>
nnoremap <silent> <ESC>\ :TmuxNavigatePrevious<cr>

map <silent> <ESC>H :vertical resize +5<cr>
map <silent> <ESC>L :vertical resize -5<cr>
map <silent> <ESC>J :resize +5<cr>
map <silent> <ESC>K :resize -5<cr>

noremap <Leader>a :Ack <cword><cr>
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
noremap <Leader>gb :Gblame<cr>

nmap <Leader>cw <ESC>
nmap <Leader>s <Plug>(easymotion-overwin-f)
nmap <C-f> <Plug>(easymotion-overwin-f)
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
map w <Plug>CamelCaseMotion_w
map b <Plug>CamelCaseMotion_b
map e <Plug>CamelCaseMotion_e
map <D-/> <C-_><C-_>
map Y y$
map <C-s> <ESC>:w<CR>
map <S-w> <ESC>:q!<CR>
map <F5> :setlocal spell! spelllang=de_de,en_us<CR>


nnoremap ,i i_<Esc>r
imap jj <Esc><Esc>
imap <C-s> <ESC>:w<CR>
imap <C-g> <Plug>IMAP_JumpForward

vnoremap K :<C-u>call <sid>VisualAck()<cr>
vnoremap K :<C-u>call <sid>VisualAck()<cr>

cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
cnoremap <silent> x<cr> call QuitPrompt()<cr>

inoremap jk <ESC>

vmap ^ $

silent! iunmap (
silent! iunmap )
silent! iunmap {
silent! iunmap }
