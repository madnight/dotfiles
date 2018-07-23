#show shell execution time needed on right prompt
ZSH_COMMAND_START=0
typeset -gF SECONDS

function preexec {
    print -rn -- $terminfo[el];
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

# just enter “cd …./dir”
rationalise-dot() {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
  else
    LBUFFER+=.
  fi
}
zle -N rationalise-dot
bindkey . rationalise-dot

# right prompt settings
local timing='$(printf "%%{$fg[cyan]%%}%.2f%%f" "$ZSH_COMMAND_TIME")'
error="%{$fg[red]%}%(?..%? )"
right=("$git" "$error" "$timing" "$vi_mode_prompt_info"  )

GIT_PROMPT_EXECUTABLE="haskell"
# GIT_PROMPT_EXECUTABLE="python"

# Default values for the appearance of the prompt. Configure at will.
ZSH_THEME_GIT_PROMPT_PREFIX="("
ZSH_THEME_GIT_PROMPT_SUFFIX=")"
ZSH_THEME_GIT_PROMPT_SEPARATOR="|"
ZSH_THEME_GIT_PROMPT_BRANCH="%{$fg_bold[magenta]%}"
ZSH_THEME_GIT_PROMPT_STAGED="%{$fg[red]%}%{●%G%}"
ZSH_THEME_GIT_PROMPT_CONFLICTS="%{$fg[red]%}%{✖%G%}"
ZSH_THEME_GIT_PROMPT_CHANGED="%{$fg[blue]%}%{+%G%}"
ZSH_THEME_GIT_PROMPT_BEHIND="%{↓%G%}"
ZSH_THEME_GIT_PROMPT_AHEAD="%{↑%G%}"
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{|u%G%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg_bold[green]%}%{✔%G%}"

PROMPT='$(git_super_status)%{$fg[red]%} » %{$reset_color%}'
# PROMPT='%{$fg[red]%} » %{$reset_color%}'
RPROMPT="%B%{$fg[blue]%}%~ %{$reset_color%}\$(echo \"${(pj::)right}\")"

function zle-line-init zle-keymap-select {
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
