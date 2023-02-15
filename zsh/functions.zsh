rangerShow() {
    BUFFER="ranger"; zle accept-line;
}
zle -N rangerShow

zle -N fzd{,}
bindkey '^F' fzd


upgrade() {
    echo "Server = https://archive.archlinux.org/repos/$(date -d "yesterday 13:00" +'%Y/%m/%d')/\$repo/os/\$arch" | sudo tee /etc/pacman.d/mirrorlist
    sudo pacman -Syu
}

cdUndoKey() {
  popd      > /dev/null
  zle       reset-prompt
  echo
  ls
  echo
}

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

256color() {
  for code in {0..255};
      do echo -e "\e[38;05;${code}m $code: Test";
  done
}

cd() {
    builtin cd $1 && ls;
}

disabled() {
    sudo systemctl disable $1.service;
}

facd() {
    cd $(locate -i $1 | head -n 1);
}

killall() {
    ps -ef | grep $1 | grep -v grep | awk '{print $2}' | xargs kill -9;
}

killport() {
    lsof -i tcp:8080 | grep LISTEN | awk '{print $2}' | xargs kill;
}

parentProcess () {
    ps -p "$1" -o ppid=
}

pkgfiler() {
    pacman -Ql $1 | grep bin;
}

yodacommit() {
    commit -m "$(fortune)"
}

dockerprune() {
    docker stop $(docker ps -a -q)
    sudo docker system prune -a -f;
    docker rm -vf $(docker ps -aq);
    docker rmi -f $(docker images -aq);
    docker volume prune -f;
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

function fail {
  echo $1 >&2
  exit 1
}

function retry {
  local n=1
  local max=50
  while true; do
    "$@" && break || {
      if [[ $n -lt $max ]]; then
        ((n++))
        echo "Command failed. Attempt $n/$max:"
        sleep $n;
      else
        fail "The command has failed after $n attempts."
      fi
    }
  done
}

# tranlate given string with online leo dictonary
# depends on: lynx, perl
leo() {
    onetwo='.{1,2}'
    re="$1"
    re="${re//[^abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]/.}"
    re="${re//ue/$onetwo}"
    re="${re//ae/$onetwo}"
    re="${re//oe/$onetwo}"
    re="${re//ss/$onetwo}"
    lynx -dump -nolist 'http://dict.leo.org/ende?lp=ende&lang=de&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&search='"$1"'&relink=on' | perl -n -e "print if /$re/i;" | head -20
}

bytesToHumanReadable() {
  numfmt --to=iec-i --suffix=B --padding=7 $1
}

convertsecs() {
   eval "echo $(date -ud "@${1}" +'$((%s/3600/24/356)) years $((%s/3600/24 % 356)) days %H hours %M minutes %S seconds')"
}

c() {
  awk "{print \$$1}"
}

knodestatus() {
    kubectl get nodes -o go-template='{{range .items}}{{$node := .}}{{range .status.conditions}}{{if ne .type "Ready"}}{{if eq .status "True"}}{{$node.metadata.name}}{{" "}}{{.type}}{{" "}}{{.status}}{{"\n"}}{{end}}{{else}}{{if ne .status "True"}}{{$node.metadata.name}}{{": "}}{{.type}}{{": "}}{{.status}}{{"\n"}}{{end}}{{end}}{{end}}{{end}}' | column -t
}

kexecpodmany() {
    PODS=($(kubectl get pods --no-headers -o custom-columns=":metadata.name" | grep $1))
    shift
    for i in $PODS; do
        echo "${i}"
        kubectl exec "${i}" -- "$@"
    done
}

knodeips() {
    kubectl get nodes -o jsonpath='{range .items[*]}{.metadata.name}
        {.status.addresses[?(@.type=="ExternalIP")].address}{"\n"}{end}'
}

kgetall() {
    kubectl api-resources --verbs=list -o name | xargs -P50 -n1 kubectl get -o name
}

knodepodsall() {
     kubectl get pods --all-namespaces -o json \
       | jq '.items
            | map({podName: .metadata.name, nodeName: .spec.nodeName})
            | group_by(.nodeName)
            | map({node: .[0].nodeName, pods: map(.podName)})'
}

knodepods() {
     N="${1:-$(knode)}"
     knodepodsall \
       | jq --arg NODE "$N" '.[] | select(.node == $NODE)'
}

kremoveallterminatingns() {
    # https://stackoverflow.com/questions/52369247/namespace-stuck-as-terminating-how-i-removed-it
    kubectl get namespaces | \
        grep Terminating   | \
        awk '{print $1}' | \
        xargs -I{} bash -c "kubectl get namespace {} -o json | tr -d '\n' |
        sed 's/\"finalizers\": \[[^]]\+\]/\"finalizers\": []/' |
        kubectl replace --raw /api/v1/namespaces/{}/finalize -f -"
}

kremoveterminatingns() {
    kubectl get namespaces | \
        grep Terminating   | \
        awk '{print $1}' | fzf | \
        xargs -I{} bash -c "kubectl get namespace {} -o json | tr -d '\n' |
        sed 's/\"finalizers\": \[[^]]\+\]/\"finalizers\": []/' |
        kubectl replace --raw /api/v1/namespaces/{}/finalize -f -"
}

gproject() {
    gcloud config set project $(gcloud projects list --format="value(name)" | fzf)
    gcloud compute instances list
}

kpods() {
    kubectl get pods --no-headers -o custom-columns=":metadata.name" | fzf
}

kdeployment() {
    kubectl get deployment --no-headers -o custom-columns=":metadata.name" | fzf
}

kconfigmap() {
    kubectl get configmap --no-headers -o custom-columns=":metadata.name" | fzf
}

kdd() {
    kubectl describe deployment/$(kdeployment)
}

kgetpvc() {
    kubectl get pvc --no-headers --no-headers -o custom-columns=":metadata.name" | fzf
}

kdp() {
    kubectl describe pod/$(kpods)
}

kgpvcy() {
    kubectl get pvc/$(kgetpvc) -o yaml
}

kgpy() {
    local POD=$(kpods)
    vim -c 'set syntax=yaml' <(kubectl get pods $POD -o yaml)
}

kgdy() {
    local DEPLOYMENT=$(kdeployment)
    vim -c 'set syntax=yaml' <(kubectl get deployment $DEPLOYMENT -o yaml)
}

kgcy() {
    local CONFIGMAP=$(kconfigmap)
    vim -c 'set syntax=yaml' <(kubectl get configmap $CONFIGMAP -o yaml)
}

kpodsns() {
    kubectl get pods -n $1 --no-headers -o custom-columns=":metadata.name" | fzf
}

kns() {
    kubectl get ns --no-headers -o custom-columns=":metadata.name" | fzf
}

knode() {
    kubectl get nodes --no-headers -o wide | awk '{print $1}' | fzf
}

kshellall(){
    local NS=$(kns)
    kubectl exec -it -n $NS $(kpodsns $NS) -- /bin/sh -c "bash"
}

kexec() {
    kubectl exec $(kpods) -- "$@"
}

kdrain() {
    kubectl drain $1 --disable-eviction --delete-local-data --ignore-daemonsets --force > /dev/null &
    kubectl drain $1 --delete-local-data --ignore-daemonsets --force
}

kdescribe() {
    kubectl describe pod/$(kpods)
}

kshowsecret() {
    local SECRET=$(kubectl get secret --no-headers -o wide | awk '{print $1}' | fzf)
    kubectl get secret $SECRET -o json | jq '.data | map_values(@base64d)'
}

kdesc() {
    kdescribe
}

kcephpw() {
    kubectl get secret rook-ceph-dashboard-password \
      -o jsonpath="{['data']['password']}" \
      | base64 --decode \
      | xclip
}

knodepvc() {
  local PVC=$(kubectl get pvc --no-headers -o custom-columns=":metadata.name" | fzf)
  local POD=$(kubectl get pod --no-headers -o custom-columns=":metadata.name" | fzf)
  local VOLUME=$(kubectl get pvc $PVC -o custom-columns=":spec.volumeName" --no-headers)
  local NODE=$(kubectl get pod $POD -o custom-columns=":spec.nodeName" --no-headers)
  ssh -t -o StrictHostKeyChecking=no root@$NODE "cd /var/lib/kubelet/plugins/kubernetes.io/csi/pv/$VOLUME/globalmount; bash"
}

klogsall() {
    local NS=$(kns)
    kubectl logs -n $NS $(kpodsns $NS) -f
}

kportall() {
    local NS=$(kns)
    kubectl port-forward \
      -n $NS $(kubectl get pods -n $NS | awk '{print $1}' | fzf) $1:$1
}

kport() {
    kubectl port-forward $(kpods) $1:$1
}

klogs() {
    local POD=$(kpods)
    local CONS=$(kubectl get pods $POD -o json | jq -r '.spec.containers[].name')
    if [ "$(echo $CONS | wc -l)" -lt "2" ]; then
        kubectl logs $POD -f
    else
        kubectl logs $POD -c $(echo $CONS | fzf) -f
    fi
}

kshell() {
    local POD=$(kpods)
    local CMD="(while :; do sleep 1h && echo ping; done) & clear; cat /etc/os-release; (bash || ash || sh);"
    local CONS=$(kubectl get pods $POD -o json | jq -r '.spec.containers[].name')
    if [ "$(echo $CONS | wc -l)" -lt "2" ]; then
        kubectl exec -it $POD "--" sh -c $CMD
    else
        kubectl exec -it $POD -c $(echo $CONS | fzf) "--" sh -c $CMD
    fi
}

krootshell() {
    local POD="$(kpods)"
    local CONS=$(kubectl get pods $POD -o json | jq -r '.spec.containers[].name')
    if [ "$(echo $CONS | wc -l)" -lt "2" ]; then
        local CON_ID=$(kubectl describe pod $POD | grep "Container ID" | cut -f3- -d/)
        local NODE=$(kubectl describe pod $POD | grep "Node:" | cut -f2 -d/)
        ssh -t -o StrictHostKeyChecking=no root@$NODE docker exec -ti -u root $CON_ID /bin/bash
    else
        local CON_ID=$(kubectl describe pod $POD | grep -C 3 "$(echo $CONS | fzf)" | grep "Container ID" | cut -f3- -d/)
        local NODE=$(kubectl describe pod $POD | grep "Node:" | cut -f2 -d/)
        ssh -t -o StrictHostKeyChecking=no root@$NODE docker exec -ti -u root $CON_ID /bin/bash
    fi
}


kdebugshell() {
    kubectl exec -it $(kpods) "--" sh -c "
      yum install -y vim bind-utils curl bash;
      apt-get update && apt-get install -y vim curl dnsutils bash;
      apk add vim bind-tools net-tools curl bash rclone;
      clear;
      cat /etc/os-release;
      (bash || ash || sh);
      "
}


kdebugpod() {
    cat <<'EOF' | kubectl create -f -
apiVersion: v1
kind: Pod
metadata:
  name: alpine-debug-pod
spec:
  containers:
    - image: alpine
      name: alpine-debug-pod
      command: ["/bin/sh"]
      args:
        - -c
        - sleep infinity
EOF
}

kdebugpodmount() {
    local PVC=$(kubectl get pvc --no-headers --no-headers -o custom-columns=":metadata.name" | fzf)
    local NODE=$(kubectl get nodes --no-headers --no-headers -o custom-columns=":metadata.name" | fzf)
    local POD_NAME=$(openssl rand -hex 5)
    cat <<EOF | kubectl create -f -
apiVersion: v1
kind: Pod
metadata:
  name: alpine-debug-pod-$POD_NAME
spec:
  volumes:
    - name: data
      persistentVolumeClaim:
        claimName: $PVC
  nodeSelector:
    kubernetes.io/hostname: $NODE
  containers:
    - image: alpine
      name: alpine-debug-pod
      command: ["/bin/sh"]
      volumeMounts:
      - mountPath: /data
        name: data
      args:
        - -c
        - apk add bind-tools && sleep infinity
EOF
}


kpvcclone() {
    if [ $# -eq 0 ]
      then return
    fi
    local PVC=$(kubectl get pvc --no-headers --no-headers -o custom-columns=":metadata.name" | fzf)
    local SIZE=$(kubectl get pvc $PVC --no-headers --no-headers -o custom-columns=":spec.resources.requests.storage")
    local CLASS=$(kubectl get pvc $PVC --no-headers --no-headers -o custom-columns=":spec.storageClassName")
    cat <<EOF | kubectl create -f -
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: $1
spec:
  dataSource:
    name: $PVC
    kind: PersistentVolumeClaim
  accessModes:
    - ReadWriteMany
  resources:
    requests:
      storage: $SIZE
  storageClassName: $CLASS
EOF
}


ke() {
    kubectl get events -A --field-selector type=Warning -o json \
      | jq '[.items | .[] | {
          message,
          reason,
          name: .metadata.name,
          host: .source.host
      }]'
}

knodeshell() {
    ssh -o StrictHostKeyChecking=no root@$(knode | awk '{print $2}')
}

kneat() {
    ((!$#)) && { kubectl-neat | bat -l yaml } || { kubectl-neat -f $1 | bat -l yaml }
}

kcephstatus() {
    local cephstatus=$(kubectl get CephCluster -A -o json \
      | jq -r '[.items[] | {
              name: .metadata.name,
              version: .spec.cephVersion.image,
              mon_count: .spec.mon.count,
              status: { details: .status.ceph.details },
              health: .status.ceph.health
            }]')
    if [[ $* == *--details* ]] then
      echo $cephstatus | jq
    else
      echo $cephstatus \
          | jq -r '.[] | "\(.name)\t\(.version)\t\(.mon_count)\t\(.health)"' \
          | column -t
    fi
}

kcephshell() {
   kubectl exec -n $1 -it $(kubectl get pods -n $1 \
     | grep "rook-ceph-tools" \
     | cut -d' ' -f1) -- /bin/bash -c "ceph status; bash"
}

knumpodspernode() {
    kubectl get pods --all-namespaces -o json \
      | jq '.items[] | .spec.nodeName' -r \
      | sort \
      | uniq -c \
      | sort -nr
}

kcrash() {
    kubectl get pods -A | grep CrashLoopBackOff
}

knodeuptimes() {
    kubectl get nodes -o wide --no-headers\
      | awk '{print $1}' \
      | xargs -n 1 -P 20 -I {} kubectl-node_shell {} -- sh -c 'echo $(hostname) $(uptime)' \
      | grep -v nsenter \
      | awk '{print $1, $4, $5}' \
      | tr -d "," \
      | column -t
}

knodepod() {
    kubectl get pod -o=custom-columns=NODE:.spec.nodeName,NAME:.metadata.name
}
alias knodepods=knodepod
alias knp=knodepod

kexecnode() {
    ssh -o StrictHostKeyChecking=no root@$(knode | awk '{print $2}') -C "$@"
}

kexecnodes(){
    kgn --no-headers \
      | awk '{print $1}' \
      | xargs -I {} kubectl-node_shell {} -- sh -c "$@"
}

kswitchns() {
    kubectl config set-context --current --namespace=$(kns)
    kubectl get pods
}

alias ksn=kswitchnamespace

kswitchcontext() {
    kubectl config get-contexts --no-headers \
      | awk '{print $2}' \
      | fzf \
      | xargs -I {} kubectl config use-context {}
}

alias kswitchctx=kswitchcontext
alias ksctx=kswitchcontext
alias ksc=kswitchcontext

calc () {
    echo "$*" | bc -l;
}

randomstring() {
    strings /dev/urandom | grep -o '[[:alnum:]]' | head -n "${1:-30}" | tr -d '\n'; echo
}

alias randstr=randomstring

man() {
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
            *.tar.zst)   tar --use-compress-program=unzstd -xvf;;
            *)           echo "don't know how to extract '$1'..." ;;
        esac
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
    lynx -dump "http://www.google.com/search?hl=en&q=$1" \
      | grep -E "Did\ you\ mean\ to\ search|instead"
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

function countdown() {
   date1=$((`date +%s` + $1));
   while [ "$date1" -ge `date +%s` ]; do
     echo -ne "$(date -u --date @$(($date1 - `date +%s`)) +%H:%M:%S)\r";
     sleep 0.1
   done
}

function timer() {
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
  pid=$(ps ax | sed 1d | fzf -m | awk '{print $1}')
  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

ghc-with() {
  nix-shell -p "haskellPackages.ghcWithPackages (ps: with ps; [ $* ])"
}

lualatex-nonstopmode() {
while inotifywait --event modify "$1" || true; do
  lualatex --halt-on-error "$1"
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
