#!/bin/sh
#
#                               _|            _|    _|
#                     _|    _|      _|_|_|        _|_|_|_|  _|  _|_|    _|_|_|
#                       _|_|    _|  _|    _|  _|    _|      _|_|      _|
#                     _|    _|  _|  _|    _|  _|    _|      _|        _|
#                     _|    _|  _|  _|    _|  _|      _|_|  _|          _|_|_|

# wallpaper
feh --bg-scale groot-i-am-root-qhd.jpg

# x settings
xset r rate 250 50
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
# disable terminal bell
xset -b
xsetroot -cursor_name left_ptr &
source ~/.xprofile

# sync primary clipboard and GTK clipboard
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"
export $(dbus-launch)

DEFAULT_SESSION='bspwm'
# DEFAULT_SESSION='xfce'
# DEFAULT_SESSION='i3 --shmlog-size 0'

xmodmap -e "clear Lock"
xmodmap -e "keycode 9 = NoSymbol"
xmodmap -e "keycode 66 = Escape NoSymbol Escape"

# startxfce4
bspwm
