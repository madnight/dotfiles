
#		       _       _ _
#		 __  _(_)_ __ (_) |_ _ __ ___
#		 \ \/ / | '_ \| | __| '__/ __|
#		  >  <| | | | | | |_| | | (__
#		 /_/\_\_|_| |_|_|\__|_|  \___|


# it executes the *.sh files in /etc/X11/xinit/xinitrc.d/
# which currently contains a systemd owned file (50-systemd-user.sh)
# that makes the right call to dbus-update-activation-environment.
# This is better than calling dbus-.. manually,
# as it will be updated if other dbus stuff changes...
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
        for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
                [ -x "$f" ] && . "$f"
        done
        unset f
fi

# wallpaper
# WP=$HOME/Pictures/xubuntu-trusty.png
# set wallpaper on three monitors
# feh --bg-scale $WP $WP $WP
if [[ $(hostname) == 'thinkpad' ]]; then
    feh --bg-scale /home/x/Pictures/hollywood.jpg
fi

if [[ $(hostname) == 't480s' ]]; then
    feh --bg-scale /home/x/pictures/rocket_launch.jpg /home/x/pictures/sunset.jpg /home/x/pictures/hollywood.jpg
fi

# x settings
xset r rate 800 50
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash

setxkbmap de

# disable terminal bell
xset -b

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

xmodmap $HOME/.Xmodmap

exec bspwm
# exec startxfce4
xterm &
