space(){
   inside_git_repo="$(git rev-parse --is-inside-work-tree 2>/dev/null)"
   if [ "$inside_git_repo" ]; then
    echo "$(echo -n '%k')%F{red}»"
   else
    echo "$(echo -n '%k')%F{red} »"
   fi
}

POWERLEVEL9K_CUSTOM_SPACE="space"

export POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND='white'
export POWERLEVEL9K_COMMAND_EXECUTION_TIME_PRECISION=3
export POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=1
export POWERLEVEL9K_EXECUTION_TIME_ICON=''
export POWERLEVEL9K_FAIL_ICON=x
export POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(virtualenv pyenv vcs)
export POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(dir status root_indicator background_jobs command_execution_time kubecontext)
export POWERLEVEL9K_STATUS_CROSS=true
export POWERLEVEL9K_STATUS_OK=false
export POWERLEVEL9K_VCS_GIT_HOOKS=(vcs-detect-changes git-remotebranch git-tagname)
export POWERLEVEL9K_VCS_UNSTAGED_ICON='*'

typeset -g POWERLEVEL9K_BACKGROUND='none'
typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_FOREGROUND='red'
typeset -g POWERLEVEL9K_DIRENV_FOREGROUND=8
typeset -g POWERLEVEL9K_DIR_BACKGROUND='none'
typeset -g POWERLEVEL9K_DIR_FOREGROUND=8
typeset -g POWERLEVEL9K_INSTANT_PROMPT=off
typeset -g POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR="$(space)"
typeset -g POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR='>'
typeset -g POWERLEVEL9K_PYENV_FOREGROUND=8
typeset -g POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR=''
typeset -g POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR='<'
typeset -g POWERLEVEL9K_STATUS_ERROR_FOREGROUND=red
typeset -g POWERLEVEL9K_STATUS_OK_FOREGROUND=green
typeset -g POWERLEVEL9K_VCS_CLEAN_FOREGROUND=010
typeset -g POWERLEVEL9K_VCS_LOADING_FOREGROUND=244
typeset -g POWERLEVEL9K_VCS_MODIFIED_FOREGROUND=011
typeset -g POWERLEVEL9K_VCS_UNTRACKED_FOREGROUND=011
typeset -g POWERLEVEL9K_VIRTUALENV_FOREGROUND=8
typeset -g POWERLEVEL9K_VIRTUALENV_SHOW_PYTHON_VERSION=false
typeset -g POWERLEVEL9K_KUBECONTEXT_SHOW='gcloud|kubectl|helm|kubens|kubectx|oc|istioctl|kogito|k9s|helmfile'

export ZLE_RPROMPT_INDENT=0

source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
