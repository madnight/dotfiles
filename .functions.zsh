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

for c in $user_commands; do alias sc-$c="systemctl $c"; done
for c in $sudo_commands; do alias sc-$c="sudo systemctl $c"; done

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

killport() { lsof -i tcp:$1 | awk 'NR!=1 {print $2}' | xargs kill; }

cd() { builtin cd $1 && ls; }

facd() { cd $(locate -i $1 | head -n 1); }

fan() { nano $(locate -i $1 | head -n 1); }

sfan() { sudo vim $(fa $1 | head -n 1); }

maketar() { tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; }

makezip() { zip -r "${1%%/}.zip" "$1" ; }

pkgfiler() { pacman -Ql $1 | grep bin; }

f() { find $(pwd) | grep $1; }

h() { if [ -z "$*" ]; then history; else history | egrep "$@"; fi }

clip() { echo "$@" | xclip; }

mkcdir() { /bin/mkdir -p "$@" && cd "$_"; }

256color() { for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done }

# some funny git stuff
# yodagit() { git commit -a -m '$(fortune)' && git push }
# yodacommit() { git commit -a -m '$(fortune)' && git push }
yoda() { git add . && git commit && git push; }

pagrep() 
{
    [[ -z "$1"  ]] && echo 'Define a grep string and try again' && return 1
    find $(pwd) -type f | parallel -k -j150% -n 1000 -m grep -H -n "$1" {}
}

cpstat () { tar cf - "$1" | pv | (cd "$2";tar xf -) }

dls () { echo `ls -l | grep "^d" | awk '{ print $9 }' | tr -d "/"`; }

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
    echo "done";
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
            *.tar.xz)    tax xf $1 	;;
            *.bz2)       bunzip2 $1     ;;
            *.rar)       unrar x $1     ;;
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
        dict)          	vim ~/.conky/dict;;
        weather)       	vim ~/.conky/conky_weather/weather_5days;;
        wiki)		vim ~/.conky/wiki;;
        irc)		vim ~/.conky/irc;;
        grey)   	vim ~/.conky/conkyrc_grey;;
        mail)   	vim ~/.conky/mail;;
        hc)	    	vim ~/.config/herbstluftwm/autostart;;
        compton)   	vim ~/.config/compton.conf;;
        autostart)      vim ~/.config/herbstluftwm/autostart;;
        log)   	        vim ~/.conky/log;;
        news)   	vim ~/.conky/news;;
        i3)             vim ~/.i3/config;;    
        status)         vim ~/.i3status.conf;;    
        vim)            vim ~/.vimrc;;
        res)            vim ~/.Xresources && xrdb ~/.Xresources;;
        def)            vim ~/.Xdefaults && xrdb ~/.Xdefaults;;
        ncm)            vim ~/.ncmpcpp/config;;
        mutt)           vim ~/.mutt/muttrc;;
        x)              vim ~/.xinitrc;;
        termite)        vim ~/.config/termite/config;;
        *)              echo "Unknown application: $1";;
    esac
}


# conky shortcuts
conk() {
    case $1 in
        dict)          	conky -c ~/.conky/dict &;;
        mail)          	conky -c ~/.conky/mail &;;
        weather)       	conky -c ~/.conky/conky_weather/weather_5days &;;
        wiki)		conky -c ~/.conky/wiki &;;
        grey)   	conky -c ~/.conky/conkyrc_grey &;;
        irc)   	  	conky -c ~/.conky/irc &;;
        log)   	  	conky -c ~/.conky/log &;;
        news)   	conky -c ~/.conky/news &;;
        *)              echo "Unknown application: $1";;
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
