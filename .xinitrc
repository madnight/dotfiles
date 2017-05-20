#!/bin/sh
#                               _|            _|    _|
#                     _|    _|      _|_|_|        _|_|_|_|  _|  _|_|    _|_|_|
#                       _|_|    _|  _|    _|  _|    _|      _|_|      _|
#                     _|    _|  _|  _|    _|  _|    _|      _|        _|
#                     _|    _|  _|  _|    _|  _|      _|_|  _|          _|_|_|

# Executed by startx (run your window manager from here)

# wallpaper
feh --bg-scale groot-i-am-root-qhd.jpg

xset r rate 250 50
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b             # disable terminal bell

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

xsetroot -cursor_name left_ptr &

xrdb -merge ~/.Xresources
xrdb -merge ~/.Xdefaults

# sync between the primary X11 clipboard and the GTK clipboard
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

[ -f ~/.xprofile ] && source ~/.xprofile

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

export $(dbus-launch)

sxhkd -c Git/dotfiles/.config/sxhkd/sxhkdrc &

DEFAULT_SESSION='bspwm'
# DEFAULT_SESSION='i3 --shmlog-size 0'

xmodmap -e "clear Lock"
xmodmap -e "keycode 9 = NoSymbol"
xmodmap -e "keycode 66 = Escape NoSymbol Escape"

case $1 in
  bspwm         ) exec dbus-launch bspwm-session ;;
  cinnamon      ) exec dbus-launch cinnamon-session ;;
  gnome         ) exec dbus-launch gnome-session ;;
  i3|i3wm       ) exec dbus-launch i3 --shmlog-size 0 ;;
  xfce          ) exec dbus-launch xfce4-session ;;
  *             ) exec dbus-launch $DEFAULT_SESSION ;;
esac

