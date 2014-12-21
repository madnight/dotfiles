
#                          _|                                              _|_|  _|            
#      _|_|_|_|    _|_|_|  _|_|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
#          _|    _|_|      _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
#        _|          _|_|  _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
#      _|_|_|_|  _|_|_|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
#                                                                                          _|  
#                                                                                      _|_|    

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=1000
bindkey -e
zstyle :compinstall filename '/home/x/.zshrc'
autoload -Uz compinit promptinit colors
compinit
promptinit
colors
bindkey -v

bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^R' history-substring-search-up

#xrdb ~/.Xdefaults
if [ "$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')" = xterm ]; then 
  echo -e -n "\x1b[\x36 q" # changes to steady bar
fi

# File not found hook: https://wiki.archlinux.org/index.php/Pkgfile
source /usr/share/doc/pkgfile/command-not-found.zsh
# Syntax Highlighting (Green/Yellow/Red) Colors for Bash Commands
source ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /usr/share/git/completion/git-prompt.sh
source ~/.zshrc_priv
#PS1='[%n@%m %c$(__git_ps1 " (%s)")]\$ '

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias i='sudo pacman -S' 
alias is='s apt-cache search'
alias ii='s apt-get install -f'
alias a='s add-apt-repository'
alias ar='s add-apt-repository --remove'
alias u='sudo pacman -Syu'
alias apps='thunar /usr/share/applications/'
alias r='sudo pacman -R'
alias rr='s apt-get -y autoremove'
alias fa='echo "maybe updatedb?" && locate -i'
alias findall='locate -i'
alias translate='t'
alias calc='speedcrunch'
alias c='clear'
alias n='nano'
alias nemo='nemo $(pwd) '
alias snemo='sudo nemo $(pwd) '
alias p='echo "USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND" && ps aux | grep --color=auto'
alias v='vim --remote'
alias history='history 1'
alias x='xrandr --display :0 --output DVI-I-3 --auto'
alias archey='archey3'
alias cdd='cd /home/datadisk/Download'
alias parallel='parallel --no-notice'
alias stresstest='parallel ::: "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999"'
alias shutdown='s shutdown -h now'
alias vlcp='vlc *.mkv'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias df="pydf"
alias xclip="xclip -selection c"
alias ya="yaourt --noconfirm"
alias aur="yaourt --noconfirm"
alias m="mousepad"
alias eb="vim /home/x/.zshrc"
alias e="extract"
alias listp="pacman -Qit | grep 'Name\|Description\|Required By\|Depends On\|Build Date\|Install Date\|Install Reason\|^$'"
alias listpall="pacman -Qi | grep 'Name\|Description\|Required By\|Depends On\|Build Date\|Install Date\|Install Reason\|^$'"
alias hc='herbstclient'
alias k='killall -9'
alias bbc1='nvlc ~/Radio/bbc1.pls'
alias sysinfo="inxi -F"
alias cpu-z="inxi -F"
alias cpuz="inxi -F"
alias xchat="LANGUAGE=en_US.UTF-8:en:de_DE.UTF-8:de xchat"
alias crc="conky -c /home/x/.conky/conkyrc_grey &"
alias chess="parallel ::: 'vim /home/datadisk/Dropbox/Dropbox/chess' 'sleep 0.8 && wmctrl -r chess -e 0,200,150,1520,800'"
alias run="dmenu_run"
alias cp='cp -i'
alias mv='mv -i'
alias rm='rm -I'                    # 'rm -i' prompts for every file
alias rm=' timeout 3 rm -Iv --one-file-system'
alias ln='ln -i'
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'
alias cls=' echo -ne "\033c"'       # clear screen for real (it does not work in Terminology)
alias :q=' exit'
alias :Q=' exit'
alias :x=' exit'
alias cd..='cd ..'
alias nano='vim'
alias lastp='yaourt -Q --date'
alias log='cat /var/log/pacman.log'
alias -g G='| egrep'
alias lyrics='sh ~/scripts/lyrics'
alias kernellog='dmesg'
alias lasti='yaourt -Q --date'
alias lastinstalled='yaourt -Q --date'
alias removeorphans='sudo pacman -Rns $(pkg-list_true_orphans)'
alias journalctl='journalctl --no-pager -n 2000'
alias mocp='mocp -T red_theme'
alias pacbackup='cd /var/cache/pacman/pkg && ls'
alias lastinstalled='yaourt -Q --date'
alias swi-prolog='swipl'
alias vimt='vim -c "NERDTree" $1'
alias svim='sudo vim'
alias android-connect="mtpfs -o allow_other /media/YOURMOUNTPOINT"
alias android-disconnect="fusermount -u /media/YOURMOUNTPOINT"
alias colors='colortest'
alias inet='ping 8.8.8.8'
alias pkill='pkill -f'
alias radio='urxvt -name ncmpcpp -e ncmpcpp'
alias music='urxvt -name ncmpcpp -e ncmpcpp'
alias ncmpcpp='urxvt -name ncmpcpp -e ncmpcpp'
alias isearch='pagrep'
alias ifind='pagrep'
alias infile='pagrep'
alias infilefind='pagrep'
alias infilesearch='pagrep'
alias mail='mutt'
alias soundcontrol='pavucontrol'
alias trainer='cd /home/x/Git/Cornamix/http/trainer/wordpress/wp-content/plugins/trainerportal/classes'
alias listdate='yaourt -Q --date'
alias rmspaces="rename ' ' '_' * && rename ' ' '_' * rename ' ' '_' * && rename ' ' '_' *" # repeat for each space
alias web='cd /home/x/Git/Cornamix/http/'
alias image='ristretto'
alias img='ristretto'
alias copy='rsync -avh -progress'
alias cp='acp -g'
alias mv='amv -g'
alias moveup='mv * .[^.]* ..'
alias fonts='xlsfonts'
alias fc-list='xlsfonts'
alias grep="/usr/bin/grep $GREP_OPTIONS"
alias md5='md5sum'
alias hash='md5sum'
#alias vim='vim --remote-tab'

unset GREP_OPTIONS

export ARCHFLAGS="-arch x86_64"
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
export JAVA_FONTS=/usr/share/fonts/TTF
export LANGUAGE=en_US.UTF-8:en
export EDITOR="vim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
export TCLLIBPATH=~/.local/share/tktheme

#Whats this? ^^
stty -ixon

PROMPT="
%{$fg[red]%} »  %{$reset_color%}"
#PROMPT="
#%{$fg[red]%} >  %{$reset_color%}"
RPROMPT="%B%{$fg[black]%}%~%{$reset_color%}"

if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
  exec startx
fi

# systemd shortcuts
start() { sudo systemctl start $1.service; sudo systemctl status $1.service; }
stop() { sudo systemctl stop $1.service; sudo systemctl status $1.service; }
restart() { sudo systemctl restart $1.service; sudo systemctl status $1.service; }
status() { sudo systemctl status $1.service; }
enabled() { sudo systemctl enable $1.service;  }
disabled() { sudo systemctl disable $1.service;  }

# one-liners
grepp() { [ $# -eq 1 ] && perl -00ne "print if /$1/i" || perl -00ne "print if /$1/i" < "$2"; }

statusdd () { watch -n5 'sudo kill -USR1 $(pgrep ^dd)'; }

cl() { cd $1 && pwd && ls; }

google() { chromium "http://www.google.com/search?q= $1"; }

t() { dict -d fd-eng-deu $1 | awk '{ if ( NR != 2 && NR != 3 && NR != 4) { print } }'; } 

rt() { dict -d fd-deu-eng $1 | awk '{ if ( NR != 2 && NR != 3 && NR != 4) { print } }'; }

cd() { builtin cd $1 && ls }

facd() { cd $(fa $1 | head -n 1) }

fan() { nano $(fa $1 | head -n 1) }

sfan() { sudo vim $(fa $1 | head -n 1) }

maketar() { tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; }

makezip() { zip -r "${1%%/}.zip" "$1" ; }

pkgfiler() { pacman -Ql $1 | grep bin }

f() { find $(pwd) | grep $1 }

h() { if [ -z "$*" ]; then history; else history | egrep "$@"; fi }

clip() { echo "$@" | xclip }

256color() { for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done }

pagrep() {
  [[ -z "$1"  ]] && echo 'Define a grep string and try again' && return 1
  find $(pwd) -type f | parallel -k -j150% -n 1000 -m grep -H -n "$1" {}
}

function cpstat() {
local pid="${1:-$(pgrep -xn cp)}" src dst
[[ "$pid" ]] || return
while [[ -f "/proc/$pid/fd/3" ]]; do
  read src dst < <(stat -L --printf '%s ' "/proc/$pid/fd/"{3,4})
  (( src )) || break
  printf 'cp %d%%\r' $((dst*100/src))
  sleep 1
done
echo
}

analyse() {
  xdotool search --desktop 0 "Xboard" key ctrl+shift+c && /home/x/Chess/stocktest "$(xclip -o)" | grep ponder | head -1 | awk '{print $ 2}' | xclip && xdotool search --desktop 0 "Xboard" key $(xclip -o | cut -c1-1) && sleep 0.2 && xdotool search --desktop 0 "Type a" key $(xclip -o | cut -c2-2) && xdotool search --desktop 0 "Type a" key $(xclip -o | cut -c3-3) && xdotool search --desktop 0 "Type a" key $(xclip -o | cut -c4-4) && xdotool search --desktop 0 "Type a" key Return
}


lastdir() {
  last_dir="$(ls -Frt | grep '/$' | tail -n1)"
  if [ -d "$last_dir" ]; then
    cd "$last_dir"
  fi
}

csource() {
  [[ $1 ]]    || { echo "Missing operand" >&2; return 1; }
  [[ -r $1 ]] || { printf "File %s does not exist or is not readable\n" "$1" >&2; return 1; }
  local output_path=${TMPDIR:-/tmp}/${1##*/};
  gcc "$1" -o "$output_path" && "$output_path";
  rm "$output_path";
  return 0;
}

cycle() {
  last_dir="$(ls -Frt | grep '/$' | tail -n1)"
  if [ -d "$last_dir" ]; then
    cd "$last_dir"
  fi
}

extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xvjf $1    ;;
      *.tar.gz)    tar xvzf $1    ;;
      *.tar.xz)    tax xf $1 	   ;;
      *.bz2)       bunzip2 $1     ;;
      *.rar)       unrar x $1       ;;
      *.gz)        gunzip $1      ;;
      *.tar)       tar xvf $1     ;;
      *.tbz2)      tar xvjf $1    ;;
      *.tgz)       tar xvzf $1    ;;
      *.zip)       unzip $1       ;;
      *.Z)         uncompress $1  ;;
      *.7z)        7z x $1        ;;
      *)           echo "don't know how to extract '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file!"
  fi
}

format () {
  if [ -f $1 ] ; then
    case $1 in
      *.js)     js-beautify $1 > beauty$1   ;;
      *.html)   tidy $1 > beauty$1   ;;
      *)           echo "don't know how to extract '$1'..." ;;
    esac
    rm $1;
    mv beauty$1 $1;
  else
    echo "'$1' is not a valid file!"
  fi
}

colortest() {
  T='gYw'   # The test text

  echo -e "\n                 40m     41m     42m     43m\
    44m     45m     46m     47m";

  for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' \
    '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' \
    '  36m' '1;36m' '  37m' '1;37m';
do FG=${FGs// /}
  echo -en " $FGs \033[$FG  $T  "
  for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
  do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
  done
  echo;
done
echo
}

conf() {
  case $1 in
    dict)          	vim ~/.conky/dict ;;
    weather)       	vim ~/.conky/conky_weather/weather_5days ;;
    wiki)		vim ~/.conky/wiki ;;
    irc)		vim ~/.conky/irc ;;
    grey)   	  	vim ~/.conky/conkyrc_grey ;;
    mail)   	  	vim ~/.conky/mail ;;
    hc)			vim ~/.config/herbstluftwm/autostart ;;
    compton)   	  	vim ~/.config/compton.conf ;;
    autostart)          vim ~/.config/herbstluftwm/autostart ;;
    log)   	        vim ~/.conky/log ;;
    news)   	  	vim ~/.conky/news ;;
    i3)                 vim ~/.i3/config;;    
    status)             vim ~/.i3status.conf;;    
    vim)                vim ~/.vimrc;;
    res)                vim ~/.Xresources && xrdb ~/.Xresources;;
    def)                vim ~/.Xdefaults && xrdb ~/.Xdefaults;;
    ncm)                vim ~/.ncmpcpp/config;;
    mutt)               vim ~/.mutt/muttrc;;
    x)                  vim ~/.xinitrc;;
    mpd)                sudo vim /etc/mpd.conf;;
    termite)            vim ~/.config/termite/config;;
    *)                  echo "Unknown application: $1" ;;
  esac
}


conk() {
  case $1 in
    dict)          	conky -c ~/.conky/dict &;;
    mail)          	conky -c ~/.conky/mail &;;
    weather)       	conky -c ~/.conky/conky_weather/weather_5days &;;
    wiki)		conky -c ~/.conky/wiki &;;
    grey)   	  	conky -c ~/.conky/conkyrc_grey &;;
    irc)   	  	conky -c ~/.conky/irc &;;
    log)   	  	conky -c ~/.conky/log &;;
    news)   	  	conky -c ~/.conky/news &;;
    *)                  echo "Unknown application: $1" ;;
  esac
}


orphans() {
  if [[ ! -n $(pacman -Qdt) ]]; then
    echo "No orphans to remove."
  else
    sudo pacman -Rns $(pacman -Qdtq)
  fi
}


function repeat() {
local i max
max=$1; shift;
for ((i=1; i <= max ; i++)); do  # --> C-like syntax
  eval "$@";
done
}

function tex() {
pdf=$(echo $1 | sed 's/tex/pdf/g')
log=$(echo $1 | sed 's/tex/log/g')
out=$(echo $1 | sed 's/tex/out/g')
aux=$(echo $1 | sed 's/tex/aux/g')
pdflatex $1 && rm $log; rm $out; rm $aux; mupdf $pdf
}


function showdesk() {
current_mode="$(wmctrl -m | grep 'showing the desktop')";
if [[ "${current_mode##* }" == ON ]]; then
  wmctrl -k off
else
  wmctrl -k on
fi
}


