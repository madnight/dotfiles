
 #                _                        __ _
 #        _______| |__     ___ ___  _ __  / _(_) __ _
 #       |_  / __| '_ \   / __/ _ \| '_ \| |_| |/ _` |
 #        / /\__ \ | | | | (_| (_) | | | |  _| | (_| |
 #       /___|___/_| |_|  \___\___/|_| |_|_| |_|\__, |
 #                                              |___/

START=$(date +%s.%N)

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# auto startx if display is not set
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

# prevent C-s form freezing the term / unfreeze terminal on abnormal exit state
[[ $- == *i* ]] && stty -ixon

function source_if_exist()
{
    if [[ -r $1 ]]; then
        source $1
    fi
}

##########################
# Autocompletion settings
##########################

# remove the trailing slash (usefull in ln)
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
# activate color-completion
zstyle ':completion:*:default'         list-colors ${(s.:.)LS_COLORS}
# format on completion
zstyle ':completion:*:descriptions'    format $'%{\e[0;31m%}completing %B%d%b%{\e[0m%}'
# for autocompletion with an arrow-key driven interface
zstyle ':completion:*' menu select
# find new installed binarys and offer completion
zstyle ':completion:*' rehash true
zstyle ':completion:*:*:docker:*' option-stacking yes
zstyle ':completion:*:*:docker-*:*' option-stacking yes

# fish like syntax highlighting
source_if_exist \
    /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=1000000
DEFAULT_USER="x"

bindkey -e

autoload -Uz compinit && compinit
autoload -Uz colors && colors

setopt AUTO_CD
setopt CORRECT
# setopt PROMPT_SUBST
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups

term="$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')"

#############################
# terminal specific settings
#############################
# fix <C-h> combo in xterm (it sends erase otherwise)
stty erase '^?'

##############
# Keybindings
##############
bindkey -v
bindkey '^P' vi-cmd-mode
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
bindkey -M viins 'jj' vi-cmd-mode
bindkey 'jj' vi-cmd-mode
# fish like autosuggestions key bindings
# ctrl + space accept the suggestion
bindkey '^ ' autosuggest-accept
# ctrl + return execute the suggestion
bindkey '^^m' autosuggest-execute

#################################
# make zsh vi behave more like vi
#################################

# Don't use vi mode in backward delete word/char because it cannot delete
# characters on the left of position you were in insert mode.
zle -A .backward-kill-word vi-backward-kill-word
zle -A .backward-delete-char vi-backward-delete-char

# Just delete char in command mode on backspace.
bindkey -M vicmd "^?" vi-backward-delete-char

# Disable moving one char back after switching to insert mode.
vi-esc-fix() {
  zle vi-cmd-mode
  zle forward-char
}
zle -N vi-esc-fix
bindkey -r "\e"
bindkey -M viins "\e" vi-esc-fix

# Hit e in command mode to edit current command line.
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd e edit-command-line

# prevent ignoring kl0$ keys in case you hit escape
bindkey -M vicmd "\e0" vi-beginning-of-line
bindkey -M vicmd "\e$" vi-end-of-line
bindkey -M vicmd "\ej" down-history
bindkey -M vicmd "\ek" up-history
bindkey -M vicmd "\e[1~" vi-end-of-line
bindkey -M vicmd "\e[4~" vi-beginning-of-line
bindkey -M vicmd "${terminfo[khome]}" vi-beginning-of-line
bindkey -M vicmd "${terminfo[kend]}" vi-end-of-line
bindkey -M viins "\e0" vi-beginning-of-line
bindkey -M viins "\e$" vi-end-of-line
bindkey -M viins "\ej" down-history
bindkey -M viins "\ek" up-history
bindkey -M viins '^H'  backward-delete-char
bindkey -M vicmd '^H'  backward-delete-char
bindkey -M viins '^?'  backward-delete-char
bindkey -M vicmd '^Y'  yank

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

# just enter ... dots
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

# command not found hook: https://wiki.archlinux.org/index.php/Pkgfile
source_if_exist /usr/share/doc/pkgfile/command-not-found.zsh

# colorize command if valid e.g. ls (green) asd123 (red)
source_if_exist ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

########################
# ENVIORNMENT variables
########################
export ARCHFLAGS="-arch x86_64"
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"
export LLC_COLLATE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_MONETARY="en_US.UTF-8"
export LC_NUMERIC="en_US.UTF-8"
export LC_TIME="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"
export EDITOR="vim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
export CHROME_BIN=/usr/bin/chromium
export PATH=$HOME/bin:/usr/local/bin:$PATH
export FZF_DEFAULT_COMMAND='rg --files --hidden -g ""'
export GCLOUD_PROJECT=coral-firefly-151914
export GOOGLE_APPLICATION_CREDENTIALS=/home/x/.config/gcloud/application_default_credentials.json
export WEECHAT_HOME=$HOME/.config/weechat
export NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"

# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
unset MANPATH
unset GREP_OPTIONS
unsetopt HUP

#################################
# source additional zsh settings
#################################

# import prompt, aliases and functions
source_if_exist ~/zsh/keybindings.zsh
source_if_exist ~/zsh/aliases.zsh
source_if_exist ~/zsh/functions.zsh
source_if_exist ~/zsh/prompt.zsh

[ -n "$TMUX" ] && export TERM=screen-256color

# Performance Warning
END=$(date +%s.%N)
ZSHRC_PERF=$(printf %.2f $(echo "$END - $START" | bc))
if (( $ZSHRC_PERF > 0.15)); then
  echo "\033[0;31mperformance warning!"
  echo ".zshrc startup time" $ZSHRC_PERF "seconds"
fi

eval "$(direnv hook zsh)"

source_if_exist $HOME/.nix-profile/etc/profile.d/nix.sh;

# xrdb ~/.Xresources
