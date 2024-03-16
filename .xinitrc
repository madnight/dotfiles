if [ -d /etc/X11/xinit/xinitrc.d ] ; then
        for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

# faster key repeat
xset r rate 800 50
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b

# german keyboard layout
setxkbmap de

xsetroot -cursor_name left_ptr
source ~/.xprofile

# sync primary clipboard and GTK clipboard
autocutsel -fork
autocutsel -selection PRIMARY -fork

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# keyring deamon
dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh) &
export SSH_AUTH_SOCK

# set keyboard layout (use capslock as control and such)
xmodmap $HOME/.Xmodmap

exec bspwm
xterm &
