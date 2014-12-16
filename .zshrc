
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

# Syntax Highlighting (Green/Yellow/Red) Colors for Bash Commands
source ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zshrc_priv


#if [ "$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')" = xterm ]; then 
#  color00="d0/d0/d0" # Base 00 - Black
#  color01="ac/41/42" # Base 08 - Red
#  color02="90/a9/59" # Base 0B - Green
#  color03="f4/bf/75" # Base 0A - Yellow
#  color04="6a/9f/b5" # Base 0D - Blue
#  color05="aa/75/9f" # Base 0E - Magenta
#  color06="75/b5/aa" # Base 0C - Cyan
#  color07="d0/d0/d0" # Base 05 - White
#  color08="50/50/50" # Base 03 - Bright Black
#  color09=$color01   # Base 08 - Bright Red
#  color10=$color02   # Base 0B - Bright Green
#  color11=$color03   # Base 0A - Bright Yellow
#  color12=$color04   # Base 0D - Bright Blue
#  color13=$color05   # Base 0E - Bright Magenta
#  color14=$color06   # Base 0C - Bright Cyan
#  color15="f5/f5/f5" # Base 07 - Bright White
#  color16="d2/84/45" # Base 09
#  color17="8f/55/36" # Base 0F
#  color18="20/20/20" # Base 01
#  color19="30/30/30" # Base 02
#  color20="b0/b0/b0" # Base 04
#  color21="e0/e0/e0" # Base 06

# 16 color space
#  printf "\e]4;0;rgb:$color00\e\\"
#  printf "\e]4;1;rgb:$color01\e\\"
#  printf "\e]4;2;rgb:$color02\e\\"
#  printf "\e]4;3;rgb:$color03\e\\"
#  printf "\e]4;4;rgb:$color04\e\\"
#  printf "\e]4;5;rgb:$color05\e\\"
#  printf "\e]4;6;rgb:$color06\e\\"
#  printf "\e]4;7;rgb:$color07\e\\"
#  printf "\e]4;8;rgb:$color08\e\\"
#  printf "\e]4;9;rgb:$color09\e\\"
#  printf "\e]4;10;rgb:$color10\e\\"
#  printf "\e]4;11;rgb:$color11\e\\"
#  printf "\e]4;12;rgb:$color12\e\\"
#  printf "\e]4;13;rgb:$color13\e\\"
#  printf "\e]4;14;rgb:$color14\e\\"
#  printf "\e]4;15;rgb:$color15\e\\"
#fi

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
alias ge='geany'
alias parallel='parallel --no-notice'
alias stresstest='parallel ::: "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999"'
alias shutdown='s shutdown -h now'
alias vlcp='vlc *.mkv'
alias cp='cp -avr'
#alias cp='echo use pv instead!!! example: "pv source.jpg > /folder/target.jpg"'
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
alias copy='clip'
alias hc='herbstclient'
alias k='killall -9'
alias bbc1='nvlc ~/Radio/bbc1.pls'
alias sysinfo="inxi -F"
alias cpu-z="inxi -F"
alias cpuz="inxi -F"
alias xchat="LANGUAGE=en_US.UTF-8:en:de_DE.UTF-8:de xchat"
alias crc="conky -c /home/x/.conky/conkyrc_grey &"
alias chess="parallel ::: 'geany /home/datadisk/Dropbox/Dropbox/chess' 'sleep 0.8 && wmctrl -r chess -e 0,200,150,1520,800'"
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
#alias vim='vim --remote-tab'
alias grep="/usr/bin/grep $GREP_OPTIONS"
unset GREP_OPTIONS

export ARCHFLAGS="-arch x86_64"
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
export JAVA_FONTS=/usr/share/fonts/TTF
export LANGUAGE=en_US.UTF-8:en
export EDITOR="vim"
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

start() { sudo systemctl start $1.service; sudo systemctl status $1.service; }
stop() { sudo systemctl stop $1.service; sudo systemctl status $1.service; }
restart() { sudo systemctl restart $1.service; sudo systemctl status $1.service; }
status() { sudo systemctl status $1.service; }
enabled() { sudo systemctl enable $1.service;  }
disabled() { sudo systemctl disable $1.service;  }

grepp() 
{ 
  [ $# -eq 1 ] && perl -00ne "print if /$1/i" || perl -00ne "print if /$1/i" < "$2";
}

statusdd () 
{ 
  watch -n5 'sudo kill -USR1 $(pgrep ^dd)'; 
}

cl() 
{ 
  cd $1 && pwd && ls; 
}

google() 
{ 
  chromium "http://www.google.com/search?q= $1"; 
}

t() 
{ 
  dict -d fd-eng-deu $1 | awk '{ if ( NR != 2 && NR != 3 && NR != 4) { print } }'; 
}

rt() 
{ 
  dict -d fd-deu-eng $1 | awk '{ if ( NR != 2 && NR != 3 && NR != 4) { print } }'; 
}

pagrep() {
  [[ -z "$1"  ]] && echo 'Define a grep string and try again' && return 1
  find $(pwd) -type f | parallel -k -j150% -n 1000 -m grep -H -n "$1" {}

}

256color() {
  for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done
}

x() {
  if [[ -f "$1" ]]; then
    case "$1" in
      *.tar.lrz)
        b=$(basename "$1" .tar.lrz)
        lrztar -d "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.lrz)
        b=$(basename "$1" .lrz)
        lrunzip "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.tar.bz2)
        b=$(basename "$1" .tar.bz2)
        tar xjf "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.bz2)
        b=$(basename "$1" .bz2)
        bunzip2 "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.tar.gz)
        b=$(basename "$1" .tar.gz)
        tar xzf "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.gz)
        b=$(basename "$1" .gz)
        gunzip "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.tar.xz)
        b=$(basename "$1" .tar.xz)
        tar Jxf "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.xz)
        b=$(basename "$1" .gz)
        xz -d "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.rar)
        b=$(basename "$1" .rar)
        unrar e "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.tar)
        b=$(basename "$1" .tar)
        tar xf "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.tbz2)
        b=$(basename "$1" .tbz2)
        tar xjf "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.tgz)
        b=$(basename "$1" .tgz)
        tar xzf "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.zip)
        b=$(basename "$1" .zip)
        unzip "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.Z)
        b=$(basename "$1" .Z)
        uncompress "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *.7z)
        b=$(basename "$1" .7z)
        7z x "$1" && [[ -d "$b" ]] && cd "$b" ;;
      *) echo "don't know how to extract '$1'..." && return 1;;
    esac
    return 0
  else
    echo "'$1' is not a valid file!"
    return 1
  fi
}

analyse() 
{
  xdotool search --desktop 0 "Xboard" key ctrl+shift+c && /home/x/Chess/stocktest "$(xclip -o)" | grep ponder | head -1 | awk '{print $ 2}' | xclip && xdotool search --desktop 0 "Xboard" key $(xclip -o | cut -c1-1) && sleep 0.2 && xdotool search --desktop 0 "Type a" key $(xclip -o | cut -c2-2) && xdotool search --desktop 0 "Type a" key $(xclip -o | cut -c3-3) && xdotool search --desktop 0 "Type a" key $(xclip -o | cut -c4-4) && xdotool search --desktop 0 "Type a" key Return
}

f() 
{
  find $(pwd) | grep $1
}

h() { if [ -z "$*" ]; then history; else history | egrep "$@"; fi; }


clip() 
{
  echo "$@" | xclip
}


lastdir()
{
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

cycle()
{
  last_dir="$(ls -Frt | grep '/$' | tail -n1)"
  if [ -d "$last_dir" ]; then
    cd "$last_dir"
  fi
}

extract () 
{
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


colortest()
{
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

conf() 
{
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
    *)                  echo "Unknown application: $1" ;;
  esac
}


conk() 
{
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


orphans() 
{
  if [[ ! -n $(pacman -Qdt) ]]; then
    echo "No orphans to remove."
  else
    sudo pacman -Rns $(pacman -Qdtq)
  fi
}


function repeat()     
{
  local i max
  max=$1; shift;
  for ((i=1; i <= max ; i++)); do  # --> C-like syntax
    eval "$@";
  done
}

function tex()
{
  pdf=$(echo $1 | sed 's/tex/pdf/g')
  log=$(echo $1 | sed 's/tex/log/g')
  out=$(echo $1 | sed 's/tex/out/g')
  aux=$(echo $1 | sed 's/tex/aux/g')
  pdflatex $1 && rm $log; rm $out; rm $aux; mupdf $pdf
}


function cd()
{
  builtin cd $1 && ls
}

function facd()
{
  cd $(fa $1 | head -n 1)
}

function fan()
{
  nano $(fa $1 | head -n 1)
}

function sfan()
{
  sudo nano $(fa $1 | head -n 1)
}

function showdesk()
{
  current_mode="$(wmctrl -m | grep 'showing the desktop')";
  if [[ "${current_mode##* }" == ON ]]; then
    wmctrl -k off
  else
    wmctrl -k on
  fi
}

function maketar() 
{ 
  tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; 
}

function makezip() 
{
  zip -r "${1%%/}.zip" "$1" ; 
}

function pkgfiler() 
{
  pacman -Ql $1 | grep bin
}

