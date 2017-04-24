nnoremap <C-j> <C-w>j
nnoremap K :Ack! '<C-r><C-w>'<cr>
nnoremap <Leader>sb :CtrlPBuffer<CR>
nnoremap <leader>cd :lcd %:p:h<CR>
nnoremap <SPACE> :
nnoremap <leader>n :NERDTreeToggle<CR>
nnoremap <C-L> :nohl<CR><C-L>
" remove trailing white spaces
nnoremap <Leader>rw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>dw :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>
nnoremap <Leader>fu :CtrlPFunky<Cr>
nnoremap <Leader>fU :execute 'CtrlPFunky ' . expand('<cword>')<Cr>
nnoremap <silent> <C-p> :CtrlPMixed<CR>
nnoremap <silent> <C-r> :earlier<CR>
nnoremap <silent> <C-e> :WinResizerStartResize<CR>
nnoremap ZZ :call QuitPrompt()<cr>

cnoremap <silent> q<cr>  call QuitPrompt()<cr>
cnoremap <silent> wq<cr> call QuitPrompt()<cr>
cnoremap <silent> x<cr> call QuitPrompt()<cr>

noremap <Leader>a :Ack <cword><cr>
noremap <Leader>s :Ack
noremap <C-X> :bd<CR>
noremap <silent> <c-up> :call SwapUp()<CR>
noremap <silent> <c-down> :call SwapDown()<CR>
" noremap <C-Right> :MBEbn<CR>
noremap <C-Right> :bnext<CR>
" noremap <C-Left> :MBEbp<CR>
noremap <C-Left> :bprevious<CR>
noremap <Leader>rn :call NumberToggle()<CR>

" fugitive shortcuts
noremap <Leader>gs :Gstatus<cr>
noremap <Leader>gc :Gcommit<cr>
noremap <Leader>ga :Gwrite<cr>
noremap <Leader>gl :Glog<cr>
noremap <Leader>gd :Gdiff<cr>
noremap <Leader>gb :Gblame<cr>

map <C-PageUp> <C-w>wh<CR>
map <C-PageDown> <C-w>wl<CR>
map w <Plug>CamelCaseMotion_w
map b <Plug>CamelCaseMotion_b
map e <Plug>CamelCaseMotion_e

inoremap jk <ESC>

nmap s <Plug>(easymotion-bd-f)
nmap <Leader>rv <ESC>:so ~/.vimrc<CR>
nmap <Leader>v <ESC>:so ~/.vimrc<CR>
nmap <Leader>cw <ESC>:ChooseWin<CR>
let g:choosewin_overlay_enable = 1
nmap ya y$
nmap <S-k> <C-w>wl<CR>
nmap <S-w> :bd<CR>
" Jump faster
nmap <C-j> 4j
nmap <C-h> <C-w>h
nmap <M-l> <C-w>l
" Alt / Mod Key (A-,M-) := <ESC>
nmap <ESC>l <C-w>l
nmap <ESC>h <C-w>h
nmap <ESC>j <C-w>j
nmap <ESC>k <C-w>k
" Jump to definition (ctags -R)
nmap <C-b> <C-]>
nmap o o<ESC>
nmap <C-k> 4k
nmap <C-a> :A<CR>
nmap <C-o> O<Esc>
nmap ^ $
nmap zz ZZ
nmap <C-g> <Plug>IMAP_JumpForward

imap ii <Esc>
imap jj <Esc>
imap <C-s> <ESC>:w<CR>
imap <C-g> <Plug>IMAP_JumpForward

vmap ^ $
vnoremap K :<C-u>call <sid>VisualAck()<cr>

map <D-/> <C-_><C-_>
map Y y$
map <C-s> <ESC>:w<CR>
map <S-w> <ESC>:q!<CR>
map <F5> :setlocal spell! spelllang=de_de,en_us<CR>

nnoremap <silent> {Left-Mapping} :TmuxNavigateLeft<cr>
nnoremap <silent> {Down-Mapping} :TmuxNavigateDown<cr>
nnoremap <silent> {Up-Mapping} :TmuxNavigateUp<cr>
nnoremap <silent> {Right-Mapping} :TmuxNavigateRight<cr>
nnoremap <silent> {Previous-Mapping} :TmuxNavigatePrevious<cr>
