
"                     _                           _
"          __ _ _   _| |_ ___   ___ _ __ ___   __| |
"         / _` | | | | __/ _ \ / __| '_ ` _ \ / _` |
"        | (_| | |_| | || (_) | (__| | | | | | (_| |
"         \__,_|\__,_|\__\___/ \___|_| |_| |_|\__,_|
"

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
