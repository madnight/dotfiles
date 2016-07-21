
#                          _|                                              _|_|  _|            
#      _|_|_|_|    _|_|_|  _|_|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
#          _|    _|_|      _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
#        _|          _|_|  _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
#      _|_|_|_|  _|_|_|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
#                                                                                          _|  
#                                                                                      _|_|    

# reload xdefaults
xrdb ~/.Xdefaults

# activate color-completion
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}
# format on completion
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=10000
DEFAULT_USER="x"

bindkey -e
autoload -Uz compinit && compinit
autoload -Uz promptinit && promptinit
autoload -Uz colors && colors
# User configuration
export PATH=$HOME/bin:/usr/local/bin:$PATH

setopt AUTO_CD
setopt CORRECT
setopt PROMPT_SUBST
setopt completealiases
setopt correctall
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups

# for autocompletion with an arrow-key driven interface
zstyle ':completion:*' menu select
# find new installed binarys and offer completion
zstyle ':completion:*' rehash true

# set bg color
echo -ne "\033]11;#181715\007"

# set fg color
echo -ne "\033]10;#bea492\007" 

bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^R' history-substring-search-up
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# for rxvt home and end 
bindkey "\e[8~" end-of-line
bindkey "\e[7~" beginning-of-line
bindkey "\e[3~" delete-char

# change xterm cursor to steady bar
if [ "$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')" = xterm ]; then 
    echo -e -n "\x1b[\x36 q" 
fi

# command not found hook: https://wiki.archlinux.org/index.php/Pkgfile
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

# colorize command if valid e.g. ls (green) asd123 (red)
source ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# ENVIORNMENT variables
export ARCHFLAGS="-arch x86_64"
export LC_ALL="en_US.UTF-8"
export EDITOR="vim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
export TCLLIBPATH=~/.local/share/tktheme
#export LANG=en_US.UTF-8
unset GREP_OPTIONS

# auto startx if display is not set
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

# fany history search via C-r
function exists { which $1 &> /dev/null; }
if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }
    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi

# prevent ctrl s form freezing the term
stty -ixon 

# private aliases and functions suchs as backup
source ~/.zshrc_priv

# import prompt, aliases and functions
source ~/.prompt.zsh
source ~/.aliases.zsh
source ~/.functions.zsh

