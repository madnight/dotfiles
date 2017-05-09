
"         _
"        | | _____ _   _ ___
"        | |/ / _ \ | | / __|
"        |   <  __/ |_| \__ \
"        |_|\_\___|\__, |___/
"                  |___/

nnoremap <Leader>l :b#<cr>
nnoremap <Leader>q :q<cr>
nnoremap <Leader>wq :wq<cr>
nnoremap <Leader>bd :bd<cr>
nnoremap <Leader>vl :VimuxInterruptRunner<cr>:VimuxRunLastCommand<cr>
nnoremap <Leader>vk :VimuxInterruptRunner<cr>
nnoremap K :Ack! '<C-r><C-w>'<cr>
nnoremap <C-j> <C-w>j
nnoremap K :Ack! '<C-r><C-w>'<cr>
nnoremap <Leader>sb :CtrlPBuffer<CR>
" serach Most recently used (MRU) files (native vim function [oldfiles])
nnoremap <C-m> :History<CR>
nnoremap <Leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <Leader>n :NERDTreeToggle<CR>
nnoremap <C-L> :nohl<CR><C-L>
" remove trailing white spaces
nnoremap <Leader>rw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>dw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>fu :CtrlPFunky<Cr>
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
nnoremap <silent> <C-e> :WinResizerStartResize<CR>
nnoremap <silent> <C-p> :Files<CR>
nnoremap ZZ :call QuitPrompt()<cr>
" Alt / Mod Key (A-,M-) := <ESC>
nnoremap <silent> <ESC>h :TmuxNavigateLeft<cr>
nnoremap <silent> <ESC>l :TmuxNavigateRight<cr>
nnoremap <silent> <ESC>k :TmuxNavigateDown<cr>
nnoremap <silent> <ESC>j :TmuxNavigateUp<cr>
nnoremap <silent> <ESC>\ :TmuxNavigatePrevious<cr>

noremap <Leader>a :Ack <cword><cr>
noremap <Leader>s :Ack
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
noremap <Leader>gd :Gdiff<cr>
noremap <Leader>gb :Gblame<cr>

nmap <Leader>cw <ESC>
nmap s <Plug>(easymotion-bd-f)
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
nmap <C-o> O<Esc>
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

imap ii <Esc>
imap jj <Esc>
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

