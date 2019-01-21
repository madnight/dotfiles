packagelist=(
abiword
ack
adobe-source-code-pro-fonts
alsa-utils
arandr
archey3
aria2
aspell
aspell-de
aspell-en
aurvote
autoconf
autocutsel
autofs
automake
awesome-terminal-fonts
bash
bash-completion
bat
bc
bdf-tewi-git
bdf-unifont
bdf-zevv-peep
bind-tools
binutils
bless
bspwm
bzip2
ca-certificates
chromium
cloc
cmake
colordiff
command-not-found
compton
coreutils
cowsay
cppcheck
cpplint
cpulimit
create_ap
cryptsetup
ctags
curl
cython
device-mapper
dhcpcd
dialog
diff-so-fancy
diffutils
dina-font
direnv
dnsmasq
docker
docker-compose
dunst
elinks
espeak
exa
exfat-utils
exo
expac
fakeroot
fbset
fd
fdupes
feh
ffmpeg
figlet
filezilla
findutils
firefox
font-manager
fzf
gawk
gcc
gcolor2
gcr
gdb
geo-bash
ghc
git-extras
gitg
gnome-keyring
gnu-netcat
gnuplot
go
gohufont
gohufont-powerline
gparted
grep
grive
grub
gscreenshot
gst-libav
gst-plugins-bad
gst-plugins-base-libs
gst-plugins-good
gst-plugins-ugly
gstreamer
gstreamer-vaapi
gv
gvfs-afc
gvfs-google
gvfs-smb
gvim
gzip
hdparm
hsetroot
htop
httpie
hunspell
hunspell-de
hunspell-en_US
hwinfo
i3lock-fancy
iana-etc
icdiff
iftop
imagemagick
imgur
inetutils
inkscape
inotify-tools
inxi
iotop
iproute2
ipscan
iptables
iputils
irqbalance
iw
ix
jansson
jbig2dec
jq
keepassx2
keyutils
kodi
less
links
lm_sensors
logrotate
lostfiles
lsb-release
lsof
lxappearance
lxrandr
lynx
m4
make
man-db
man-pages
meld
mpv
mupdf
mutt
ncdu
ncmpcpp
ncurses
nerd-fonts-complete
net-tools
netctl
netdata
nethogs
networkmanager-openvpn
nload
nm-connection-editor
nmap
npm
ntp
numix-gtk-theme
openssh
openvpn
pacutils
parallel
parcellite
parted
pavucontrol
pcmanfm
pinta
pkg_scripts
pkgbrowser
pkgfile
playerctl
polybar
powerline
pptpclient
pulseaudio
pulseaudio-alsa
pulseaudio-ctl
pv
pygmentize
python
python-virtualenv
ranger
redshift-git
redshift-gtk-git
reflector
ripgrep
ristretto
rofi
rsync
rxvt-unicode
scrot
siji-git
speedcrunch
stow
stress
sxhkd
tamsyn-font
tamzen-font-git
tar
terminus-font
termite
thunar
tig
tk
tmux
tor-browser
trayer
ttf-consolas-powerline
ttf-droid
ttf-google-fonts-git
ttf-liberation
ttf-ms-fonts
ttf-nerd-fonts-symbols
ufetch
unclutter
unrar
urxvt
usbutils
util-linux
veracrypt
vi
vim
vim-spell-de
vim-spell-en
vlc
vnstat
weechat
wget
which
whois
wireshark-cli
wpa_supplicant
xautolock
xcb-util-xrm
xclip
xdo
xdotool
xorg-font-utils
xorg-server
xorg-xmodmap
xscreensaver
xsel
xterm
xz
yarn
youtube-dl
zathura
zenity
zip
zsh
zsh-pure-prompt
zsh-git-prompt
zsh-syntax-highlighting
)

for package in "${packagelist[@]}"
do
    if ! pacman -Qs $package > /dev/null ; then
        echo -ne "\nINSTALL $package\n"
        sudo pacman --noconfirm -S $package || yay $package
    fi
done