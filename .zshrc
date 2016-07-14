
#                          _|                                              _|_|  _|            
#      _|_|_|_|    _|_|_|  _|_|_|          _|_|_|    _|_|    _|_|_|      _|            _|_|_|  
#          _|    _|_|      _|    _|      _|        _|    _|  _|    _|  _|_|_|_|  _|  _|    _|  
#        _|          _|_|  _|    _|      _|        _|    _|  _|    _|    _|      _|  _|    _|  
#      _|_|_|_|  _|_|_|    _|    _|        _|_|_|    _|_|    _|    _|    _|      _|    _|_|_|  
#                                                                                          _|  
#                                                                                      _|_|    

HISTFILE=~/.histfile
HISTSIZE=100000
SAVEHIST=10000
bindkey -e
zstyle :compinstall filename '/home/x/.zshrc'
autoload -Uz compinit promptinit colors
setopt PROMPT_SUBST

xset s off -dpms
ZSH=/usr/share/oh-my-zsh/
DEFAULT_USER="x"

#ZSH_THEME="robbyrussell"
#ZSH_THEME="random"
ZSH_THEME="agnoster"

# Uncomment the following line to disable bi-weekly auto-update checks.
DISABLE_AUTO_UPDATE="true"

plugins=(git history-substring-search)

# User configuration
export PATH=$HOME/bin:/usr/local/bin:$PATH

ZSH_CACHE_DIR=$HOME/.oh-my-zsh-cache
if [[ ! -d $ZSH_CACHE_DIR ]]; then
    mkdir $ZSH_CACHE_DIR
fi

#source $ZSH/oh-my-zsh.sh
source /usr/share/oh-my-zsh/oh-my-zsh.sh 

setopt AUTO_CD
setopt CORRECT
setopt completealiases
setopt append_history
setopt share_history
setopt hist_verify
setopt hist_ignore_all_dups
zstyle ':completion:*' menu select
zstyle ':completion:*' rehash true
setopt completealiases


# set bg color
echo -ne "\033]11;#181715\007"

# set fg color
echo -ne "\033]10;#bea492\007" 

bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^R' history-substring-search-up
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

# change cursor to steady bar
if [ "$(ps -f -p $(cat /proc/$(echo $$)/stat | cut -d \  -f 4) | tail -1 | sed 's/^.* //')" = xterm ]; then 
    echo -e -n "\x1b[\x36 q" 
fi

# File not found hook: https://wiki.archlinux.org/index.php/Pkgfile
source /usr/share/doc/pkgfile/command-not-found.zsh

source ~/scripts/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

source ~/.zshrc_priv

prompt minimal

source ~/.oh-my-zsh/themes/agnoster.zsh-theme


# show shell execution time needed on right prompt
ZSH_COMMAND_START=0
typeset -gF SECONDS

function preexec {
    ZSH_COMMAND_START=${ZSH_COMMAND_START:-$SECONDS}
}

function precmd {
    if [[ -n "$ZSH_COMMAND_START" ]]; then
        ((ZSH_COMMAND_TIME = SECONDS - ZSH_COMMAND_START))
        unset ZSH_COMMAND_START
    else
        ZSH_COMMAND_TIME=0
    fi
}

local timing='$(
printf "%%{$fg[black]%%}%.2f%%f" "$ZSH_COMMAND_TIME"
)'

xrdb /home/x/.Xdefaults

local left right

left=("$prefix" "$mode" "$username" "$hostname" "$cwd" "$finale")
right=("$git" "$timing")

RPS1="\$(echo \"${(pj::)right}\")"

RPROMPT="%B%{$fg[black]%}%~ %{$reset_color%}\$(echo \"${(pj::)right}\")"

# aliases
alias android-connect="mtpfs -o allow_other /media/YOURMOUNTPOINT"
alias android-disconnect="fusermount -u /media/YOURMOUNTPOINT"
alias apps='thunar /usr/share/applications/'
alias archey='archey3'
alias ar='s add-apt-repository --remove'
alias a='s add-apt-repository'
alias aurpk='yaourt -G'
alias aur="yaourt --noconfirm"
alias awe='cd /home/x/Git/awe15-04'
alias bbc1='nvlc ~/Radio/bbc1.pls'
alias calc='speedcrunch'
alias catkin_make='catkin_make -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so'
alias catkin_make_isolated='catkin_make_isolated -DPYTHON_EXECUTABLE=/usr/bin/python2 -DPYTHON_INCLUDE_DIR=/usr/include/python2.7 -DPYTHON_LIBRARY=/usr/lib/libpython2.7.so'
alias cat="pygmentize -g"
alias c='clear'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias cd..='cd ..'
alias cdd='cd /home/datadisk/Download'
alias chgrp='chgrp --preserve-root'
alias chmod='chmod --preserve-root'
alias chown='chown --preserve-root'
alias cls=' echo -ne "\033c"'     
alias copylast='fc -ln -1 | awk '\''{$1=$1}1'\'' | pbcopy'
alias copy='rsync -avh -progress'
alias count='ls -la | wc -l'
alias cp='acp -g'
alias cp='cp -i'
alias cplast='fc -ln -1 | awk '\''{$1=$1}1'\'' | pbcopy'
alias cpu-z="inxi -F"
alias cpuz="inxi -F"
alias crc="conky -c /home/x/.conky/conkyrc_grey &"
alias df="pydf"
alias diff='git diff HEAD~1'
alias dnet='sudo killall dhcpcd; sudo dhcpcd; ping 8.8.8.8'
alias eb="vim /home/x/.zshrc"
alias e="extract"
alias error='journalctl -p 0..3 -xn'
alias fa='echo "maybe updatedb?" && locate -i '
alias fastwget='aria2c -x 16' 
alias fc-list='xlsfonts'
alias ffcurl="curl -H \"User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.8) Gecko/2009032609 Firefox/3.0.0 (.NET CLR 3.5.30729)\""
alias findall='locate -i'
alias fonts='xlsfonts'
alias ft='parallel -k rspec -c ::: spec/controllers/agreements_controller_spec.rb spec/controllers/project_spec.rb spec/controllers/work_package_controller_spec.rb spec/controllers/employee_spec.rb spec/controllers/team_controller_spec.rb spec/controllers/organizations_controller_spec.rb spec/controllers/user_spec.rb'
alias -g C='| wc -l'
alias -g G='| egrep'
alias gaa='git add .'
alias gba='git branch -a'
alias gb='git remote update origin --prune && git branch -a'
alias gc='git commit'
alias gco='git checkout'
alias gdob='git push origin --delete'
alias gitm="git commit -m"
alias glb='git branch -a'
alias gp='git pull'
alias gpsu='git push --set-upstream'
alias gpsuo='git push --set-upstream origin'
unset GREP_OPTIONS
alias grep="/usr/bin/grep $GREP_OPTIONS"
alias gr='git reset'
alias gs='git status'
alias hash='md5sum'
alias hc='herbstclient'
alias history='history 1'
alias iecurl="curl -H \"User-Agent: Mozilla/5.0 (Windows; U; MSIE 6.0; Windows NT 5.1; SV1; .NET CLR 2.0.50727)\""
alias ifind='pagrep'
alias ii='s apt-get install -f'
alias image='ristretto'
alias img='ristretto'
alias inet='ping 8.8.8.8'
alias infilefind='pagrep'
alias infile='pagrep'
alias infilesearch='pagrep'
alias iomonitor='watch -n 0.1 iostat'
alias iowatch='iomonitor'
alias isearch='pagrep'
alias is='s apt-cache search'
alias i='sudo pacman -S' 
alias journalctl='journalctl --no-pager -n 2000'
alias kernellog='dmesg'
alias k='killall -9'
alias la='ls -A'
alias lastinstalled='yaourt -Q --date'
alias lastinstalled='yaourt -Q --date'
alias lasti='yaourt -Q --date'
alias lastp='yaourt -Q --date'
alias listdate='yaourt -Q --date'
alias listpall="pacman -Qi | grep 'Name\|Description\|Required By\|Depends On\|Build Date\|Install Date\|Install Reason\|^$'"
alias listp="pacman -Qit | grep 'Name\|Description\|Required By\|Depends On\|Build Date\|Install Date\|Install Reason\|^$'"
alias lit='cd /home/x/Git/MA_FabianBeuke/literature'
alias ll='ls -alF'
alias l='ls -CF'
alias ln='ln -i'
alias log='cat /var/log/pacman.log'
alias lsof='lsof -Pni'
alias lyrics='sh ~/scripts/lyrics'
alias ma='cd /home/x/Git/MA_FabianBeuke/thesis/src/'
alias mail='mutt'
alias md5='md5sum'
alias m="mousepad"
alias mocp='mocp -T red_theme'
alias mount='sudo mount -o umask=0,uid=nobody,gid=nobody'
alias moveup='mv * .[^.]* ..'
alias music='urxvt -name ncmpcpp -e ncmpcpp'
alias mv='amv -g'
alias mv='mv -i'
alias nano='vim'
alias ncmpcpp='urxvt -name ncmpcpp -e ncmpcpp'
alias nemo='nemo $(pwd) '
alias n='nano'
alias pacbackup='cd /var/cache/pacman/pkg && ls'
alias parallel='parallel --no-notice'
alias pdf='evince'
alias p='echo "USER       PID %CPU %MEM    VSZ   RSS TTY      STAT START   TIME COMMAND" && ps aux | grep --color=auto'
alias ped='cd /home/x/Git/MA_FabianBeuke/src/pedsim'
alias pkill='pkill -f'
alias ports='sudo lsof -Pni'
alias pres='cd /home/x/Git/MA_FabianBeuke/presentation'
alias :q=' exit'
alias :Q=' exit'
alias radio='urxvt -name ncmpcpp -e ncmpcpp'
alias rdr='rake db:migrate:reset && rake db:seed'
alias rds='time rake db:seed'
alias removeorphans='sudo pacman -Rns $(pkg-list_true_orphans)'
alias rm=' timeout 3 rm -Iv --one-file-system'
alias rm='rm -i'    
alias rmspaces="rename ' ' '_' * && rename ' ' '_' * rename ' ' '_' * && rename ' ' '_' *" 
alias rr='s apt-get -y autoremove'
alias r='sudo pacman -R'
alias run="dmenu_run"
alias setdate='timedatectl'
alias settime='timedatectl'
alias shutdown='s /usr/bin/systemctl poweroff'
alias smallfiles='sudo find / -xdev -type d -size +100k'
alias snemo='sudo nemo $(pwd) '
alias soundcontrol='pavucontrol'
alias sshx='ssh -XC -c blowfish-cbc,arcfour'
alias stack='howdoi -c -n 3'
alias stresstest='parallel ::: "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999" "pi 999999999"' 
alias svim='sudo vim'
alias swi-prolog='swipl'
alias sysinfo="inxi -F"
alias syslog='cat /var/log/everything.log'
alias tab='xterm -e "java -jar /home/x/scripts/RemoteDroidServer/RemoteDroidServer.jar"'
alias tests='rspec -f d -c'
alias tex='texnonstop'
alias trainer='cd /home/x/Git/Cornamix/http/trainer/wordpress/wp-content/plugins/trainerportal/classes'
alias translate='t'
alias trash="cd ~/.local/share/Trash/files/"
alias t='rspec -f d -c'
alias u='sudo pacman -Syu'
alias vimt='vim -c "NERDTree" $1'
alias vim='vim'
alias vlcp='vlc *.mkv'
alias vlc='vlc'
alias v='vim --remote'
alias watchdir='watch -n 1 ls -lh'
alias web='cd /home/x/Git/Cornamix/http/'
alias :x=' exit'
alias xchat="LANGUAGE=en_US.UTF-8:en:de_DE.UTF-8:de xchat"
alias xclip="xclip -selection c"
alias x='xrandr --display :0 --output DVI-I-3 --auto'
alias ya="yaourt --noconfirm"


# ENV vars
export ARCHFLAGS="-arch x86_64"
#export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on'
#export _JAVA_OPTIONS='-Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel' 
#export JAVA_FONTS=/usr/share/fonts/TTF
#export LANG=en_US.UTF-8
export LC_ALL="en_US.UTF-8"
export EDITOR="vim"
export BROWSER="chromium"
export SHELL=/usr/bin/zsh
export TCLLIBPATH=~/.local/share/tktheme

# auto startx if display is not set
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    exec startx
fi

# systemd shortcuts
start() { sudo systemctl start $1.service; sudo systemctl status $1.service; }
stop() { sudo systemctl stop $1.service; sudo systemctl status $1.service; }
restart() { sudo systemctl restart $1.service; sudo systemctl status $1.service; }
status() { sudo systemctl status $1.service; }
enabled() { cd /usr/lib/systemd/system; sudo systemctl enable $1.service;  }
disabled() { sudo systemctl disable $1.service;  }
user_commands=(
list-units is-active status show help list-unit-files
is-enabled list-jobs show-environment cat)

sudo_commands=(start stop reload restart try-restart isolate kill
reset-failed enable disable reenable preset mask unmask
link load cancel set-environment unset-environment
edit)

for c in $user_commands; do; alias sc-$c="systemctl $c"; done
for c in $sudo_commands; do; alias sc-$c="sudo systemctl $c"; done

alias failed='systemctl --failed'
alias killall='killall -s SIGKILL'

# one-liners
grepp() { [ $# -eq 1 ] && perl -00ne "print if /$1/i" || perl -00ne "print if /$1/i" < "$2"; }

statusdd () { watch -n5 'sudo kill -USR1 $(pgrep ^dd)'; }

cl() { cd $1 && pwd && ls; }

google() { chromium "http://www.google.com/search?q=$1"; }

github() { chromium "https://github.com/search?q=$1"; }

asm () 
{
    nasm -f elf $1
    ld -o $1 $1.o -melf_i386
    rm $1.o
    echo "Done building, the file 'echo' is your executable"
}

# backup and list packages
packages () 
{
    pacman -Qqe  >| /home/datadisk/Dropbox/ArchBackup/pkglist_$(date +%F).txt 
    pacman -Qqe  
}

# tranlate given string with online leo dictonary
# depends on: lynx, perl
leo()
{
    onetwo='.{1,2}'
    re="$1"
    re="${re//[^abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]/.}"
    re="${re//ue/$onetwo}"
    re="${re//ae/$onetwo}"
    re="${re//oe/$onetwo}"
    re="${re//ss/$onetwo}"
    lynx -dump -nolist 'http://dict.leo.org/ende?lp=ende&lang=de&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&search='"$1"'&relink=on' | perl -n -e "print if /$re/i;" | head -20
}

killport() { lsof -i tcp:$1 | awk 'NR!=1 {print $2}' | xargs kill }

cd() { builtin cd $1 && ls }

facd() { cd $(locate -i $1 | head -n 1) }

fan() { nano $(locate -i $1 | head -n 1) }

sfan() { sudo vim $(fa $1 | head -n 1) }

maketar() { tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; }

makezip() { zip -r "${1%%/}.zip" "$1" ; }

pkgfiler() { pacman -Ql $1 | grep bin }

f() { find $(pwd) | grep $1 }

h() { if [ -z "$*" ]; then history; else history | egrep "$@"; fi }

clip() { echo "$@" | xclip }

mkcdir() { /bin/mkdir -p "$@" && cd "$_"; }

256color() { for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done }

# some funny git stuff
# yodagit() { git commit -a -m '$(fortune)' && git push }
# yodacommit() { git commit -a -m '$(fortune)' && git push }
yoda() { git add . && git commit && git push }

pagrep() 
{
    [[ -z "$1"  ]] && echo 'Define a grep string and try again' && return 1
    find $(pwd) -type f | parallel -k -j150% -n 1000 -m grep -H -n "$1" {}
}

cpstat () { tar cf - "$1" | pv | (cd "$2";tar xf -) }

dls () { echo `ls -l | grep "^d" | awk '{ print $9 }' | tr -d "/"` }

installfont() {
    sudo cp $1 /usr/share/fonts/misc/
    sudo mkfontdir /usr/share/fonts/misc
    xset +fp /usr/share/fonts/misc
    xlsfonts | grep $1
}

fontcache() {
    sudo echo -n "Updating font cache... "
    xset +fp ~/.fonts
    sudo fc-cache >/dev/null -f
    sudo mkfontscale /usr/share/fonts/TTF
    sudo mkfontdir   /usr/share/fonts/TTF
    echo done
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

# copy with md check
md5copy() {
    echo "usage: md5copy Star_Trek.mkv /run/media/x/stick/"
    rsync -c -h --stats --info=progress2 $1 $2
    parallel md5sum ::: $1 $2$1
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

restartsound () {
    pulseaudio --kill;
    pulseaudio --start;
    pacmd list-sinks;
    pacmd set-default-sink alsa_output.pci-0000_00_14.2.analog-stereoi;
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

findbin() {
    for ARG in $(pacman -Qql $1); do
        [ ! -d $ARG ] && [ -x $ARG ] && echo $ARG;
    done
}

colortest() {
    T='gYw'   
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
echo;
}

# config shortucts
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


# conky shortcuts
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

# synonyme search
function syn() {
    BROWSER="/usr/bin/lynx -source" 
    WEBSITE="http://thesaurus.reference.com/search?q=$1" 
    HTML2TEXT="/usr/bin/html2text -style compact" 
    if test $1; then 
        lynx -source 'http://www.thesaurus.com/browse/'"$1"'?s=' | html2text
    else 
        echo "Usage: $0 word" 
        exit 1 
    fi
}

# tex compile and clean up command
function tex() {
    pdf=$(echo $1 | sed 's/tex/pdf/g')
    log=$(echo $1 | sed 's/tex/log/g')
    out=$(echo $1 | sed 's/tex/out/g')
    aux=$(echo $1 | sed 's/tex/aux/g')
    toc=$(echo $1 | sed 's/tex/toc/g')
    lof=$(echo $1 | sed 's/tex/lof/g')
    lot=$(echo $1 | sed 's/tex/lot/g')
    bbl=$(echo $1 | sed 's/tex/bbl/g')
    blg=$(echo $1 | sed 's/tex/blg/g')
    dvi=$(echo $1 | sed 's/tex/dvi/g')
    fbd=$(echo $1 | sed 's/tex/fdb\_latexmk/g')
    fls=$(echo $1 | sed 's/tex/fls/g')
    ps=$(echo $1 | sed 's/tex/ps/g')
    tdo=$(echo $1 | sed 's/tex/tdo/g')
    rm $log; rm $out; rm $aux; rm $toc; rm $lof; rm $lot;
    rm $bbl; rm $blg; rm $dvi; rm $fdb; rm $fls; rm $ps; rm $tdo;
    pdflatex $1;
}

function texnonstop() {
    latexmk -pvc -pdf -latex=pdflatex -interaction=nonstopmode $1
}


function showdesk() {
    current_mode="$(wmctrl -m | grep 'showing the desktop')";
    if [[ "${current_mode##* }" == ON ]]; then
        wmctrl -k off
    else
        wmctrl -k on
    fi
}

# precmd () { print -Pn "\e]2; \a" } # title bar promptinit

# backup everything from ssd to hdd
backup ()
{
    sudo rsync -aAXh --stats --info=progress2 --delete --exclude={"/dev/*","/proc/*","/sys/*","/tmp/*","/run/*","/mnt/*","/media/*","/lost+found","/home/datadisk/*","/home/datadisk2/*","/home/x/.gvfs","/home/x/Downloads/*","/var/cache/pacman/*","/home/x/.config/VirtualBox/*","/home/x/.wine/*","/home/x/.atom/*","/home/x/.winex64/*","/home/x/.thumbnails/*","/home/x/.cache/mozilla/*","/home/x/.codeintel/db/*"} /* /home/datadisk/fullarchbackup
}

colors()
{
    ( x=`tput op` y=`printf %$((${COLUMNS}-6))s`;
    for i in {0..7};
    do
        o=00$i;
        echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;
    done )
}

ix() {
    local opts
    local OPTIND
    [ -f "$HOME/.netrc" ] && opts='-n'
    while getopts ":hd:i:n:" x; do
        case $x in
            h) echo "ix [-d ID] [-i ID] [-n N] [opts]"; return;;
            d) $echo curl $opts -X DELETE ix.io/$OPTARG; return;;
            i) opts="$opts -X PUT"; local id="$OPTARG";;
            n) opts="$opts -F read:1=$OPTARG";;
        esac
    done
    shift $(($OPTIND - 1))
    [ -t 0 ] && {
    local filename="$1"
    shift
    [ "$filename" ] && {
    curl $opts -F f:1=@"$filename" $* ix.io/$id
    return
}
echo "^C to cancel, ^D to send."
  }
  curl $opts -F f:1='<-' $* ix.io/$id
}

# improved zsh history search (ctrl+r)
# depends on: percol
# https://github.com/mooz/percol#zsh-history-search
function exists { which $1 &> /dev/null }

if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || { exists tac && tac="tac" || { tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval $tac | percol --query "$LBUFFER")
        CURSOR=$#BUFFER         # move cursor
        zle -R -c               # refresh
    }

    zle -N percol_select_history
    bindkey '^R' percol_select_history
fi

# prevent ctrl s form freezing the term
stty -ixon 
