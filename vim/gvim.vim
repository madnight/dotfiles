if has('mouse')
  set mouse=a
endif

if has("gui_running")
  map <leader>r :NERDTreeFind<cr>

  set guifont=Monaco\ 15
  " set guifont=Inconsolata-g\ 15
  " set guifont=Ubuntu\ Mono\ 15
  " set guifont=Consolas\ 15
  " set guifont=Monospace\ 13

  colorscheme base16-tomorrow-night
  " colorscheme hybrid
  " colorscheme twilight
  " colorscheme github
  " colorscheme molokai

  set background=dark
  set guioptions-=T
  set guicursor=a:blinkon0
  set guioptions-=m
  set guioptions-=T
  set guioptions-=L
  set guioptions-=l
  set guioptions-=R
  set guioptions-=r
  set guioptions-=b

  " Adding automatons for when entering or leaving Vim
  au VimEnter * nested :call LoadSession()
  au VimLeave * NERDTreeClose
  au VimLeave * MBEClose
  au VimLeave * :call MakeSession()
  au VimEnter * NERDTree

  let g:nerdtree_tabs_open_on_gui_startup=0
  let g:nerdtree_tabs_open_on_new_tab=0
endif
