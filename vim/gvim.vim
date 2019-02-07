
"                     _
"          __ ___   _(_)_ __ ___
"         / _` \ \ / / | '_ ` _ \
"        | (_| |\ V /| | | | | | |
"         \__, | \_/ |_|_| |_| |_|
"         |___/
"

if has('mouse')
  set mouse=a
endif

if has("gui_running")
  map <leader>r :NERDTreeFind<cr>

  " set guifont=Gohu\ Font\ Nerd\ 11
  set guifont=Monospace

  colorscheme base16-tomorrow-night

  set background=dark
  set guioptions-=T
  set guicursor=a:blinkon0
  set guioptions-=m
  set guioptions-=T
  set guioptions-=L
  set guioptions-=l
  set guioptions-=R
  set guioptions-=r
endif
