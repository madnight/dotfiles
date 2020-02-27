rangerShow() { BUFFER="ranger"; zle accept-line; }
zle -N rangerShow

zle -N fzd{,}
bindkey '^F' fzd

cdUndoKey() {
  popd      > /dev/null
  zle       reset-prompt
  echo
  ls
  echo
}

unfreeze() { tmux send-keys C-q }

cdParentKey() {
  pushd .. > /dev/null
  zle      reset-prompt
  echo
  ls
  echo
}

zle -N             cdParentKey
zle -N             cdUndoKey
bindkey '^[.'      cdParentKey
bindkey '^[,'      cdUndoKey

# one-liners
256color() { for code in {0..255}; do echo -e "\e[38;05;${code}m $code: Test"; done }
aurless() { yaourt --noconfirm --color $1 | less -r }
cd() { builtin cd $1 && ls; }
cl() { cd $1 && pwd && ls; }
clip() { echo "$@" | xclip; }
cpstat () { tar cf - "$1" | pv | (cd "$2";tar xf -) }
disabled() { sudo systemctl disable $1.service;  }
dls () { echo `ls -l | grep "^d" | awk '{ print $9 }' | tr -d "/"`; }
duplicates() { sort $1 | uniq -cd }
enabled() { cd /usr/lib/systemd/system; sudo systemctl enable $1.service;  }
f() { find $(pwd) | grep $1; }
facd() { cd $(locate -i $1 | head -n 1); }
fan() { nano $(locate -i $1 | head -n 1); }
github() { chromium "https://github.com/search?q=$1"; }
grepp() { [ $# -eq 1 ] && perl -00ne "print if /$1/i" || perl -00ne "print if /$1/i" < "$2"; }
h() { if [ -z "$*" ]; then history; else history | egrep "$@"; fi }
killall() { ps -ef | grep $1 | grep -v grep | awk '{print $2}' | xargs kill -9; }
killport() { lsof -i tcp:8080 | grep LISTEN | awk '{print $2}' | xargs kill; }
maketar() { tar cvzf "${1%%/}.tar.gz"  "${1%%/}/"; }
makezip() { zip -r "${1%%/}.zip" "$1" ; }
mkcdir() { /bin/mkdir -p "$@" && cd "$_"; }
pagrep() { rg "$1" }
parentProcess () { ps -p "$1" -o ppid= }
pkgfiler() { pacman -Ql $1 | grep bin; }
restart() { sudo systemctl restart $1.service; sudo systemctl status $1.service; }
sfan() { sudo vim $(fa $1 | head -n 1); }
start() { sudo systemctl start $1.service; sudo systemctl status $1.service; }
status() { sudo systemctl status $1.service; }
statusdd () { watch -n5 'sudo kill -USR1 $(pgrep ^dd)'; }
stop() { sudo systemctl stop $1.service; sudo systemctl status $1.service; }
yoda() { git add -u && git commit && git push; }
yodacommit() { git commit -m "$(fortune)" }

dockerprune() {
    docker stop $(docker ps -a -q)
    sudo docker system prune -a -f;
    docker rm -vf $(docker ps -aq);
    docker rmi -f $(docker images -aq);
    docker volume prune -f;
    # sudo rm -rf /var/lib/docker/overlay2;
}

nixprune() {
   nix-env --delete-generations 14d
   nix-store --gc
   nix-collect-garbage -d
}

user_commands=(
list-units is-active status show help list-unit-files
is-enabled list-jobs show-environment cat)

sudo_commands=(start stop reload restart try-restart isolate kill
reset-failed enable disable reenable preset mask unmask
link load cancel set-environment unset-environment
edit)

for c in $user_commands; do alias sc-$c="systemctl $c"; done
for c in $sudo_commands; do alias sc-$c="sudo systemctl $c"; done

asm ()
{
    filename=$1
    nasm -f elf $1
    ld -o "${filename%.*}" "${filename%.*}.o" -melf_i386
    command rm "${filename%.*}.o"
    echo "Done building, the file '${filename%.*}' is your executable"
}

asm32()
{
    filename=$1
    nasm -f elf $1
    gcc -m32 "${filename%.*}.o" -o "${filename%.*}"
    command rm "${filename%.*}.o"
    echo "Done building, the file '${filename%.*}' is your executable"
}

# fancy history search via C-r
function exists { which $1 &> /dev/null; }
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



man()
{
    command man -t "$1" | ps2pdf - /tmp/"$1".pdf && zathura /tmp/"$1".pdf
}

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
    echo "example usage: md5copy Star_Trek.mkv /run/media/x/stick/"
    rsync -c -h --stats --info=progress2 $1 $2
    parallel md5sum ::: $1 $2$1
}

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xvjf $1    ;;
            *.tar.gz)    tar xvzf $1    ;;
            *.tar.xz)    tar xf $1  	;;
            *.xz)        unxz $1  	;;
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

printcolors() {
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

# tex compile and clean up command
function cleantex() {
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
}

function texnonstop() {
    latexmk -pvc -pdf --latex=lualatex -interaction=nonstopmode $1
}

alias g=google
function google() {
    lynx -dump "http://www.google.com/search?hl=en&q=$1" | grep -E "Did\ you\ mean\ to\ search|instead"
}

function showdesk() {
    current_mode="$(wmctrl -m | grep 'showing the desktop')";
    if [[ "${current_mode##* }" == ON ]]; then
        wmctrl -k off
    else
        wmctrl -k on
    fi
}

printcolors() {
    x=`tput op` y=`printf %$((${COLUMNS}-6))s`;
    for i in {0..7};
    do
        o=00$i;
        echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x;
    done
}

function countdown(){
   date1=$((`date +%s` + $1));
   while [ "$date1" -ge `date +%s` ]; do
     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
     sleep 0.1
   done
}

function timer(){
  date1=`date +%s`;
   while true; do
    echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r";
    sleep 0.1
   done
}

function cd() {
    new_directory="$*";
    if [ $# -eq 0 ]; then
        new_directory=${HOME};
    fi;
    builtin cd "${new_directory}" && exa
}

findbin() {
    find -type f -executable -exec file -i '{}' \; | grep 'charset=binary'
}

gdb_get_backtrace() {
    local exe=$1
    local core=$2

    gdb ${exe} \
        --core ${core} \
        --batch \
        --quiet \
        -ex "thread apply all bt full" \
        -ex "quit"
}

# screen recoding to webm best uploaded at http://webmshare.com/ (gyfact cuts at 15 sec)
record () {
    # $1 resolution $2 offset x $3 offset y $4 output example:
    # ffmpeg -f x11grab -s 1024x768 -i :0.0+10,100 -c:v libvpx -crf 12 -b:v 500K ouput.webm
    ffmpeg -f x11grab -s $1 -i :0.0+$2,$3 -c:v libvpx -crf 12 -b:v 500K $4
    # open with firefox output.webm
}

# Get a 42 chars password: generate-password 42
generate-password() {
if [[ 18 -lt $1 ]] then
  strings /dev/urandom | grep -o '[[:alnum:]]' | head -n $1 | tr -d '\n'; echo;
  else
    echo "password to short unsecure"
  fi
}

function monitor() {
    watch -n1 -t "lsof -i -n|awk '{print \$1, \$2, \$9}'|column -t";
}

########################
# fzf enhanced functions
########################
fe() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# fkill - kill process
fkill() {
  local pid
  pid=$(ps a | sed 1d | fzf -m | awk '{print $1}')
  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

ghc-with() {
  nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ $* ])"
}

lualatex-nonstopmode() {
while inotifywait --event modify "$1"; do
  lualatex "$1"
done
}

ghci-with() {
  nix-shell \
    -p "haskellPackages.ghcWithPackages (ps: with ps; [ $* ])" \
    --run ghci
}

# cd into the directory of the selected file
fcd() {
   local file
   local dir
   cd $HOME && file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

fzd() {
   local file
   local dir
   cd $HOME && file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}


# fshow - git commit browser
fshow() {
  git log --graph --color=always \
      --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
  fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
      --bind "ctrl-m:execute:
                (grep -o '[a-f0-9]\{7\}' | head -1 |
                xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                {}
FZF-EOF"
}

# fcoc - checkout git commit
fcoc() {
  local commits commit
  commits=$(git log --pretty=oneline --abbrev-commit --reverse) &&
  commit=$(echo "$commits" | fzf --tac +s +m -e) &&
  git checkout $(echo "$commit" | sed "s/ .*//")
}
# source /usr/share/fzf/key-bindings.zsh


# Key bindings
# ------------
if [[ $- == *i* ]]; then

# CTRL-T - Paste the selected file path(s) into the command line
__fsel() {
  local cmd="${FZF_ALT_C_COMMAND:-"cd $HOME && rg --files --hidden -g ''"}"
  setopt localoptions pipefail 2> /dev/null
  eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_CTRL_T_OPTS" $(__fzfcmd) -m "$@" | while read item; do
    echo -n "${(q)item} "
  done
  local ret=$?
  echo
  return $ret
}

__fzf_use_tmux__() {
  [ -n "$TMUX_PANE" ] && [ "${FZF_TMUX:-0}" != 0 ] && [ ${LINES:-40} -gt 15 ]
}

__fzfcmd() {
  __fzf_use_tmux__ &&
    echo "fzf-tmux -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf"
}

fzf-file-widget() {
  LBUFFER="${LBUFFER}$(__fsel)"
  local ret=$?
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}
zle     -N   fzf-file-widget
bindkey '^T' fzf-file-widget


fzf-cd-widget() {
  # cd $HOME
  local cmd="${FZF_ALT_C_COMMAND:-"rg --files --hidden -g ''"}"
  setopt localoptions pipefail 2> /dev/null
  local dir="$(eval "$cmd" | FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} --reverse $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS" $(__fzfcmd) +m)"
  if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  cd $(dirname "$dir")
  local ret=$?
  zle reset-prompt
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}

zle     -N    fzf-cd-widget
bindkey '^F' fzf-cd-widget

# CTRL-R - Paste the selected command from history into the command line
fzf-history-widget() {
  local selected num
  setopt localoptions noglobsubst noposixbuiltins pipefail 2> /dev/null
  selected=( $(fc -l 1 |
    FZF_DEFAULT_OPTS="--height ${FZF_TMUX_HEIGHT:-40%} $FZF_DEFAULT_OPTS --tac -n2..,.. --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS --query=${(q)LBUFFER} +m" $(__fzfcmd)) )
  local ret=$?
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle redisplay
  typeset -f zle-line-init >/dev/null && zle zle-line-init
  return $ret
}
zle     -N   fzf-history-widget
bindkey '^R' fzf-history-widget

fi
