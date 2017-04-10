
#                          _|                                              _|_|  _|            
#      _|_|_|_|    _|_|_|  _|_|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
#          _|    _|_|      _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
#        _|          _|_|  _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
#      _|_|_|_|  _|_|_|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
#                                                                                          _|  
#                                                                                      _|_|    

# reload xdefaults
[[ -e ~/.Xdefaults ]] && xrdb ~/.Xdefaults

fortune -a -s -n 200 | cowsay

# activate color-completion
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}
# format on completion
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=1000000
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
# you should not be setting the complete_aliases option 
# if you want to have completion for aliases
# setopt complete_aliases
# setopt correctall
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups

# for autocompletion with an arrow-key driven interface
zstyle ':completion:*' menu select
# find new installed binarys and offer completion
zstyle ':completion:*' rehash true

term="$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')"

if [ $term = urxvt ] || [ $term = xterm ]; then 
    # set bg color
    echo -ne "\033]11;#181715\007"
    # set fg color
    echo -ne "\033]10;#DBBCBC\007" 
fi

# change xterm cursor to steady bar
if [ $term = xterm ]; then 
    echo -e -n "\x1b[\x36 q" 
fi

# vi mode keybinding (-e for emacs)
bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^R' history-substring-search-up
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# for rxvt home and end 
bindkey "\e[1~" end-of-line
bindkey "\e[4~" beginning-of-line
#bindkey "\e[3~" delete-chabindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[khome]}" beginning-of-line
bindkey "${terminfo[kend]}" end-of-line

# by default, there is a 0.4 second delay after you hit the <ESC> key
# let's reduce this delay to 0.1 seconds.
export KEYTIMEOUT=1

# modal cursor color for vi's insert/normal modes.
zle-keymap-select () {
  if [ $KEYMAP = vicmd ]; then
    echo -ne "\033]12;6\007"
    echo -ne "\033[2 q"
  else
    echo -ne "\033]12;Grey\007"
    echo -ne "\033[4 q"
  fi
}
zle -N zle-keymap-select
zle-line-init () {
  zle -K viins
  echo -ne "\033]12;Gray\007"
  echo -ne "\033[4 q"
}
zle -N zle-line-init

# command not found hook: https://wiki.archlinux.org/index.php/Pkgfile
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && 
source /usr/share/doc/pkgfile/command-not-found.zsh

# colorize command if valid e.g. ls (green) asd123 (red)
[[ -e ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh ]] && 
source ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# fish like autosuggestions key bindings
# ctrl + space accept the suggestion
bindkey '^ ' autosuggest-accept
# ctrl + return execute the suggestion
bindkey '^^m' autosuggest-execute

# https://github.com/zsh-users/zsh-autosuggestions
# fish like autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

# ENVIORNMENT variables
export ARCHFLAGS="-arch x86_64"
export LC_ALL="en_US.UTF-8"
export EDITOR="vim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
#export TCLLIBPATH=~/.local/share/tktheme
export GEM_HOME=$(ruby -e 'print Gem.user_dir')
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
#export LANG=en_US.UTF-8
unset GREP_OPTIONS

# auto startx if display is not set
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

# fancy history search via C-r
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

# prevent C-s form freezing the term
stty -ixon 

# private aliases and functions suchs as backup
[[ -e ~/.zshrc_priv ]] && source ~/.zshrc_priv

# import prompt, aliases and functions
[[ -e ~/zsh/prompt.zsh ]] && source ~/zsh/prompt.zsh
[[ -e ~/zsh/aliases.zsh ]] && source ~/zsh/aliases.zsh
[[ -e ~/zsh/functions.zsh ]] && source ~/zsh/functions.zsh

# load tmux settings
# [[ -e ~/.tmux.conf ]] && tmux source ~/.tmux.conf

# added by travis gem
[ -f /home/x/.travis/travis.sh ] && source /home/x/.travis/travis.sh



NPM_PACKAGES="${HOME}/.npm-packages"
PATH="$NPM_PACKAGES/bin:$PATH"
# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
unset MANPATH # delete if you already modified MANPATH elsewhere in your config
export MANPATH="$NPM_PACKAGES/share/man:$(manpath)"
export CHROME_BIN=/usr/bin/chromium

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


# if [[ "$(uname -r)" != "$upacman -Q linux)" ]]; then
#  echo -e "\n\n\nkernel updated -> consider reboot"
#  uname -r;pacman -Q linux
# fi
