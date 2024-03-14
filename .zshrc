 #                _                        __ _
 #        _______| |__     ___ ___  _ __  / _(_) __ _
 #       |_  / __| '_ \   / __/ _ \| '_ \| |_| |/ _` |
 #        / /\__ \ | | | | (_| (_) | | | |  _| | (_| |
 #       /___|___/_| |_|  \___\___/|_| |_|_| |_|\__, |
 #                                               |___/


zmodload zsh/zprof
START=$(date +%s.%N)

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

for sd_cmd in systemctl systemd-analyze systemd-run; do
     alias $sd_cmd='DBUS_SESSION_BUS_ADDRESS="unix:path=$XDG_RUNTIME_DIR/bus" '$sd_cmd
done

function exist_and_not_running()
{
    if ! pgrep $1 > /dev/null; then
        if which $1 > /dev/null; then
            $@ &
        fi
    fi
}

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
source_if_exist /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source_if_exist /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh


HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=1000000
DEFAULT_USER=$(whoami)

bindkey -e

autoload -Uz compinit && compinit
autoload -Uz colors && colors

setopt AUTO_CD
setopt CORRECT
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups
# terminal specific settings
#############################
# fix <C-h> combo in xterm (it sends erase otherwise)
stty erase '^?'

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
export EDITOR="nvim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
export CHROME_BIN=/usr/bin/chromium
export PATH=$HOME/bin:/usr/local/bin:$PATH
export FZF_DEFAULT_COMMAND='rg --files --hidden -g ""'
export GCLOUD_PROJECT=coral-firefly-151914
export GOOGLE_APPLICATION_CREDENTIALS=$HOME/.config/gcloud/application_default_credentials.json
export WEECHAT_HOME=$HOME/.config/weechat
export NPM_PACKAGES="${HOME}/.npm-packages"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/bin/site_perl:/usr/bin/vendor_perl:/usr/bin/core_perl"
export NPM_PACKAGES="${HOME}/.npm-packages"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
export PATH="$NPM_PACKAGES/bin:$PATH"
export PATH=~/.composer/vendor/bin:$PATH
export ANDROID_HOME=/opt/android-sdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/platform-tools
export DEFAULT_NETWORK_INTERFACE=$(ip route | grep '^default' | awk '{print $5}' | head -n1)
export ZSH_AUTOSUGGEST_USE_ASYNC=true
[ -z "${XDG_RUNTIME_DIR}" ] && export XDG_RUNTIME_DIR=/run/user/$(id -ru)
export DBUS_SESSION_BUS_ADDRESS="unix:path=${XDG_RUNTIME_DIR}/bus"
export KUBECONFIG=$(find ~/.kube/clusters -type f | sed ':a;N;s/\n/:/;ba')

# Unset manpath so we can inherit from /etc/manpath via the `manpath` command
unset MANPATH
unset GREP_OPTIONS
unsetopt HUP

#################################
# source additional zsh settings
#################################

# import prompt, aliases and functions
source_if_exist ~/zsh/keybindings.zsh
source_if_exist ~/zsh/functions.zsh
source_if_exist ~/zsh/aliases.zsh
source_if_exist ~/zsh/prompt.zsh
source_if_exist ~/exports.zsh

[ -n "$TMUX" ] && export TERM=screen-256color

eval "$(direnv hook zsh)"

# Performance Warning
END=$(date +%s.%N)
ZSHRC_PERF=$(printf %.2f $(echo "$END - $START" | bc))
if (( $ZSHRC_PERF > 0.2)); then
  zprof | head
  echo "\033[0;31mperformance warning!"
  echo ".zshrc startup time" $ZSHRC_PERF "seconds"
fi

# interpret incoming ^H as backspace/erase
stty erase \^H
stty erase ""
bindkey '\C-?' backward-kill-word

xset r rate 600 60

zstyle ':completion:*' completer _expand_alias _complete _ignored

[[ -s "/etc/grc.zsh" ]] && source /etc/grc.zsh
