# modified agnoster theme
source ~/zsh/agnoster.zsh-theme

# agnoster modifications by overwrite
PROMPT='
%{%f%b%k%}$(build_prompt) '
build_prompt() {
# only show the agnoster git prompt in git dir
if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
  RETVAL=$?
  prompt_status
  prompt_virtualenv
  prompt_context
  # prompt dir is already on rprompt
  # prompt_dir
  prompt_git
  # we dont need mercurial
  # prompt_hg
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

# https://github.com/bhilburn/powerlevel9k/issues/319
bindkey -v
export KEYTIMEOUT=1
function zle-line-init {
  powerlevel9k_prepare_prompts
  if (( ${+terminfo[smkx]} )); then
    printf '%s' ${terminfo[smkx]}
  fi
  zle reset-prompt
  zle -R
}

function zle-line-finish {
  powerlevel9k_prepare_prompts
  if (( ${+terminfo[rmkx]} )); then
    printf '%s' ${terminfo[rmkx]}
  fi
  zle reset-prompt
  zle -R
}

function zle-keymap-select {
  powerlevel9k_prepare_prompts
  zle reset-prompt
  zle -R
}

zle -N zle-line-init
zle -N ale-line-finish
zle -N zle-keymap-select

local timing='$(printf "%%{$fg[black]%%}%.2f%%f" "$ZSH_COMMAND_TIME")'

error="%{$fg[black]%}%(?..%? )"
right=("$git" "$error" "$timing")

RPROMPT="%B%{$fg[black]%}%~ %{$reset_color%}\$(echo \"${(pj::)right}\")"
# fix right prompt indentation
ZLE_RPROMPT_INDENT=0

POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir context rbenv vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status root_indicator dir_writable background_jobs vi_mode command_execution_time)
POWERLEVEL9K_PROMPT_ON_NEWLINE=true
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=' '
POWERLEVEL9K_MULTILINE_SECOND_PROMPT_PREFIX='%F{red} »  '
POWERLEVEL9K_SHOW_CHANGESET=true
POWERLEVEL9K_PROMPT_ADD_NEWLINE=true
POWERLEVEL9K_MODE='awesome-patched'

# fix glyph spacing by adding an extra space at the end
# use the command get_icon_names for debug purpose
POWERLEVEL9K_LINUX_ICON='\uf300 '
POWERLEVEL9K_HOME_SUB_ICON='\UE18D '
POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR='\uE0B0'
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR='\uE0B2'
POWERLEVEL9K_LEFT_SEGMENT_END_SEPARATOR=''
POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR='\uE0B1'
POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR='\uE0B3'
POWERLEVEL9K_CARRIAGE_RETURN_ICON='\u21B5'
POWERLEVEL9K_ROOT_ICON='\uE801 '
# POWERLEVEL9K_RUBY_ICON='\uE847 '
# POWERLEVEL9K_AWS_ICON='\uE895 '
# POWERLEVEL9K_AWS_EB_ICON='\U1F331 '
# POWERLEVEL9K_BACKGROUND_JOBS_ICO='\uE82F '
# POWERLEVEL9K_TEST_ICON='\uE891 '
# POWERLEVEL9K_TODO_ICON='\u2611 '
# POWERLEVEL9K_BATTERY_ICON='\uE894 '
POWERLEVEL9K_DISK_ICON='\uE1AE'
POWERLEVEL9K_OK_ICON='\u2713'
POWERLEVEL9K_FAIL_ICON='\u2718 '
# POWERLEVEL9K_SYMFONY_ICON='SF '
# POWERLEVEL9K_NODE_ICON='\u2B22 '
# POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX='\u256D'$'\U2500'
# POWERLEVEL9K_MULTILINE_SECOND_PROMPT_PREFIX='\u2570'$'\U2500 '
# POWERLEVEL9K_APPLE_ICON='\uE26E '
# POWERLEVEL9K_FREEBSD_ICON='\U1F608 '
# POWERLEVEL9K_ANDROID_ICON='\uE270 '
# POWERLEVEL9K_LINUX_ICON='\uE271 '
# POWERLEVEL9K_SUNOS_ICON='\U1F31E '
POWERLEVEL9K_HOME_ICON='\uE12C'
POWERLEVEL9K_HOME_SUB_ICON='\uE18D'
POWERLEVEL9K_FOLDER_ICON='\uE818 '
POWERLEVEL9K_NETWORK_ICON='\uE1AD '
POWERLEVEL9K_LOAD_ICON='\uE190 '
POWERLEVEL9K_SWAP_ICON='\uE87D '
POWERLEVEL9K_RAM_ICON='\uE1E2 '
POWERLEVEL9K_SERVER_ICON='\uE895 '
POWERLEVEL9K_VCS_UNTRACKED_ICON='\uE16C'
POWERLEVEL9K_VCS_UNSTAGED_ICON='\uE17C'
POWERLEVEL9K_VCS_STAGED_ICON='\uE168'
POWERLEVEL9K_VCS_STASH_ICON='\uE133'
POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON='\uE1EB '
# POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON='\uE80D '
# POWERLEVEL9K_VCS_INCOMING_CHANGES_ICON='\uE131 '
POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON='\uE1EC '
# POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON='\uE80E '
# POWERLEVEL9K_VCS_OUTGOING_CHANGES_ICON='\uE132 '
POWERLEVEL9K_VCS_TAG_ICON='\uE817 '
POWERLEVEL9K_VCS_BOOKMARK_ICON='\uE87B '
POWERLEVEL9K_VCS_COMMIT_ICON='\uE821 '
POWERLEVEL9K_VCS_BRANCH_ICON='\uE0A0 '
POWERLEVEL9K_VCS_REMOTE_BRANCH_ICON='\u2192 '
POWERLEVEL9K_VCS_GIT_ICON='\uE20E'
POWERLEVEL9K_VCS_GIT_GITHUB_ICON='\uE20E'
POWERLEVEL9K_VCS_GIT_BITBUCKET_ICON='\uE20E '
POWERLEVEL9K_VCS_GIT_GITLAB_ICON='\uE20E '
# POWERLEVEL9K_VCS_HG_ICON='\uE1C3 '
POWERLEVEL9K_VCS_SVN_ICON='(svn)'
# POWERLEVEL9K_RUST_ICON=' '
POWERLEVEL9K_PYTHON_ICON='\U1F40D '
# POWERLEVEL9K_SWIFT_ICON=' '
# POWERLEVEL9K_PUBLIC_IP_ICON=' '
POWERLEVEL9K_LOCK_ICON='\UE138 '
POWERLEVEL9K_EXECUTION_TIME_ICON='\UE89C '

source  ~/powerlevel9k/powerlevel9k.zsh-theme
