
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

augroup CLClear
  " au! ColorScheme * hi clear CursorLine
augroup END

augroup vimrc_autocmd
  autocmd!
  au BufNewFile,BufRead,BufEnter *.{tex,txt} setlocal spell spelllang=de_de,en_gb
  au BufNewFile,BufRead,BufEnter *.pl set filetype=prolog
  au BufNewFile,BufRead,BufEnter *.yml set syntax=yaml
  au BufRead,BufNewFile,BufEnter *mutt* set filetype=mail
  au BufNewFile,BufRead *.coffee set filetype=coffee

  au Filetype *.js setlocal expandtab tabstop=1 shiftwidth=4 softtabstop=4

  au BufEnter *.hs set formatprg=xargs\ -0\ pointfree

  " auto change path to current file (most compatible behaviour)
  au BufEnter * silent! lcd %:p:h

  au FileType mardown set spell spelllang=en_us
  " au FileType php set omnifunc=phpcomplete#CompletePHP
  " au FileType mail setlocal spell spelllang=de_de,en_gb
  " au FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
  " au FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
  " au FileType ruby,eruby let g:rubycomplete_rails = 1
  " au FileType ruby set omnifunc=rubycomplete#Complete
  " au FileType css setlocal omnifunc=csscomplete#CompleteCSS
  " au FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  " au FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  " au FileType python setlocal omnifunc=pythoncomplete#Complete
  " au FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

  " au CursorHold * call ale#Lint()
  " au CursorHoldI * call ale#Lint()
  " au InsertLeave * call ale#Lint()
  " au TextChanged * call ale#Lint()
  " au BufDelete * if len(filter(range(1, bufnr('$')), '! empty(bufname(v:val)) && buflisted(v:val)')) == 1 | quit | endif


  au VimResized * wincmd =

  " Adding automatons for when entering or leaving Vim
  if len(argv()) < 1
    au VimEnter * nested :call LoadSession()
    au VimLeave * NERDTreeClose
    au VimLeave * MBEClose
    au VimLeave * :call MakeSession()
  endif
  " au VimEnter * NERDTree
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

" augroup load_us_ycm
  " autocmd!
  " autocmd InsertEnter * call plug#load('ultisnips', 'YouCompleteMe', 'vim-snippets', 'tmux-complete.vim')
                     " \| autocmd! load_us_ycm
" augroup END

" i want always be able to edit the files
:autocmd BufWinEnter * setlocal modifiable
