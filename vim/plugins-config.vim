
 "              _             _                              __
 "        _ __ | |_   _  __ _(_)_ __  ___    ___ ___  _ __  / _|
 "       | '_ \| | | | |/ _` | | '_ \/ __|  / __/ _ \| '_ \| |_
 "       | |_) | | |_| | (_| | | | | \__ \ | (_| (_) | | | |  _|
 "       | .__/|_|\__,_|\__, |_|_| |_|___/  \___\___/|_| |_|_|
 "       |_|            |___/
 "

" Latex settings
let g:livepreview_previewer = 'evince'
let g:Tex_CompileRule_pdf = 'latexmk -pdf'
let g:LatexBox_latexmk_options = "-pvc -pdfps"
let g:LatexBox_latexmk_preview_continuously = 1
let g:LatexBox_latexmk_async = 1

" lilydjwg/colorizer is inefficient for large files
let g:colorizer_maxlines = 200

let php_sql_query = 1
let php_htmlInStrings = 1

let Tlist_Use_Right_Window = 1

let g:acp_enableAtStartup = 0

" add jsx syntax highlights for .js files
let g:jsx_ext_required = 0

" overcome limit imposed by max height
let g:ackprg = 'rg --vimgrep'

" rg is so fast that CtrlP doesn't need to cache
" let g:ctrlp_use_caching = 0
" let g:ctrlp_funky_syntax_highlight = 1
" let g:ctrlp_show_hidden = 1
" let g:ctrlp_match_window = 'results:100'
" let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
" let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }
" let g:ctrlp_custom_ignore = '\v[\/](node_modules|target|dist)|(\.(swp|ico|git|svn))$'
" let g:ctrlp_cmd = 'CtrlPMixed'

" Airline Settings
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#buffer_min_count =2
let g:airline_theme='molokai'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
" performance optimization
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#branch#enabled = 0
let g:airline#extensions#whitespace#enabled = 0

let g:vim_tags_auto_generate = 1
let g:formatprg_js = "js-beautify"
let g:formatprg_args_js = "-i %@"

" Nerd Tree Settings
let g:NERDTreeMinimalUI = 1
let g:NERDTreeShowHidden=1
let g:NERDTreeWinPos = "left"
let g:NERDTreeMouseMode = 3
let g:NERDTreeFileExtensionHighlightFullName = 1
let g:NERDTreeExactMatchHighlightFullName = 1
let g:NERDTreePatternMatchHighlightFullName = 1
let g:NERDTreeWinSize = 40
" Add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1
" Align line-wise comment delimiters flush left instead of following code indentation
let g:NERDDefaultAlign = 'left'
" Add your own custom formats or override the defaults
let g:NERDTrimTrailingWhitespace = 1

let g:winresizer_horiz_resize = 1
let g:vim_markdown_preview_github=1

let g:hardtime_default_on = 0
let g:hardtime_maxcount = 1000

let g:UltiSnipsExpandTrigger="<C-l>"

" YouCompleteMe Settings
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_semantic_triggers =  {
  \   'c' : ['->', '.'],
  \   'objc' : ['->', '.', 're!\[[_a-zA-Z]+\w*\s', 're!^\s*[^\W\d]\w*\s',
  \             're!\[.*\]\s'],
  \   'ocaml' : ['.', '#'],
  \   'cpp,objcpp' : ['->', '.', '::'],
  \   'perl' : ['->'],
  \   'php' : ['->', '::'],
  \   'cs,java,javascript,typescript,d,python,perl6,scala,vb,elixir,go' : ['.'],
  \   'ruby' : ['.', '::'],
  \   'lua' : ['.', ':'],
  \   'erlang' : [':'],
  \ }

" Async Lint Engine Settings
let g:ale_lint_on_text_changed = 0
let g:ale_sign_error = '✖'
let g:ale_sign_warning = '--'

let g:vim_json_syntax_conceal = 0
let g:airline_skip_empty_sections = 1
let g:tmux_navigator_no_mappings = 1
let g:tmuxcomplete#trigger = 'omnifunc'
" write big choose win letters on screen
let g:choosewin_overlay_enable = 1
let g:tmux_navigator_no_mappings = 1
