# modified agnoster theme
source ~/zsh/agnoster.zsh-theme

# agnoster modifications by overwrite
PROMPT='
%{%f%b%k%}$(build_prompt) '
build_prompt() {
# only show the agnoster git prompt in git dir
if [ -d .git ]; then
  RETVAL=$?
  prompt_status
  prompt_virtualenv
  prompt_context
  # prompt dir is already on rprompt
  # prompt_dir
  prompt_git
  prompt_hg
  prompt_end
else 
# otherwise use minimal theme
 echo -ne "%{$fg[red]%} » %{$reset_color%}"
fi
}

#show shell execution time needed on right prompt
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

