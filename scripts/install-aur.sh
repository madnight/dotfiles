packagelist=(
ananicy-git
aurvote
bdf-tewi-git
bdf-zevv-peep
bind-tools
command-not-found
compton
cpplint
create_ap
direnv
font-manager
geo-bash
git-extras
gohufont
gohufont-powerline
grive
gscreenshot
hostsblock
icdiff
imgur
inxi
ipscan
ix
keepassx2
nerd-fonts-complete
numix-gtk-theme
pacman-static
pkgbrowser
pkg_scripts
polybar
preload
pulseaudio-ctl
pygmentize
siji-git
tamzen-font-git
tor-browser
ttf-consolas-powerline
ttf-google-fonts-git
ttf-ms-fonts
ufetch
un-apple-keyboard
urxvt
xorg-font-utils
zsh-git-prompt
zsh-pure-prompt
)

for package in "${packagelist[@]}"
do
    if ! pacman -Qs $package > /dev/null ; then
        echo -ne "\nINSTALL $package\n"
        yay $package
    fi
done


