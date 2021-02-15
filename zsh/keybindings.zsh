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

bindkey -s "^E" 'ls^M'
