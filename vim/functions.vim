
"          __                  _   _
"         / _|_   _ _ __   ___| |_(_) ___  _ __  ___
"        | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
"        |  _| |_| | | | | (__| |_| | (_) | | | \__ \
"        |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
"

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
