
#                          _|                                              _|_|  _|            
#      _|_|_|_|    _|_|_|  _|_|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
#          _|    _|_|      _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
#        _|          _|_|  _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
#      _|_|_|_|  _|_|_|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
#                                                                                          _|  
#                                                                                      _|_|    

# set your speed!
xset r rate 150 50

xset s off -dpms

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=10000
DEFAULT_USER="x"
ZSH=/usr/share/oh-my-zsh/
DISABLE_AUTO_UPDATE="true"

plugins=(git history-substring-search)

bindkey -e
zstyle :compinstall filename '/home/x/.zshrc'
autoload -Uz compinit promptinit colors

# User configuration
export PATH=$HOME/bin:/usr/local/bin:$PATH

ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi

source $ZSH/oh-my-zsh.sh
#source /usr/share/oh-my-zsh/oh-my-zsh.sh 

setopt AUTO_CD
setopt CORRECT
setopt completealiases
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups
setopt PROMPT_SUBST

zstyle ':completion:*' menu select
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

# change cursor to steady bar
if [ "$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')" = xterm ]; then 
    echo -e -n "\x1b[\x36 q" 
fi

# command not found hook: https://wiki.archlinux.org/index.php/Pkgfile
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

source ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# private aliases and functions suchs as backup
source ~/.zshrc_priv

prompt minimal

source ~/.oh-my-zsh/themes/agnoster.zsh-theme

# show shell execution time needed on right prompt
ZSH_COMMAND_START=0
typeset -gF SECONDS

function preexec {
    ZSH_COMMAND_START=${ZSH_COMMAND_START:-$SECONDS}
}

function precmd {
    if [[ -n "$ZSH_COMMAND_START" ]]; then
        ((ZSH_COMMAND_TIME = SECONDS - ZSH_COMMAND_START))
        unset ZSH_COMMAND_START
    else
        ZSH_COMMAND_TIME=0
    fi
}

local timing='$(
printf "%%{$fg[black]%%}%.2f%%f" "$ZSH_COMMAND_TIME"
)'

xrdb /home/x/.Xdefaults

local left right

left=("$prefix" "$mode" "$username" "$hostname" "$cwd" "$finale")
error="%{$fg[black]%}%(?..%? )"
right=("$git" "$error" "$timing")


RPROMPT="%B%{$fg[black]%}%~ %{$reset_color%}\$(echo \"${(pj::)right}\")"

unset GREP_OPTIONS

# ENVIORNMENT variables
export ARCHFLAGS="-arch x86_64"
export LC_ALL="en_US.UTF-8"
export EDITOR="vim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
export TCLLIBPATH=~/.local/share/tktheme

#export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
#export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
#export JAVA_FONTS=/usr/share/fonts/TTF
#export LANG=en_US.UTF-8


# auto startx if display is not set
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

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

# import aliases and functions
source ~/.aliases.zsh
source ~/.functions.zsh

