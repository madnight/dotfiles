prompt minimal
source ~/.oh-my-zsh/themes/agnoster.zsh-theme

# show shell execution time needed on right prompt
ZSH_COMMAND_START=0
typeset -gF SECONDS

function preexec {
    ZSH_COMMAND_START=${ZSH_COMMAND_START:-$SECONDS}
}

# execution time function
function precmd {
    if [[ -n "$ZSH_COMMAND_START" ]]; then
        ((ZSH_COMMAND_TIME = SECONDS - ZSH_COMMAND_START))
        unset ZSH_COMMAND_START
    else
        ZSH_COMMAND_TIME=0
    fi
}

local timing='$(printf "%%{$fg[black]%%}%.2f%%f" "$ZSH_COMMAND_TIME")'

error="%{$fg[black]%}%(?..%? )"
right=("$git" "$error" "$timing")

RPROMPT="%B%{$fg[black]%}%~ %{$reset_color%}\$(echo \"${(pj::)right}\")"

