
#		       _       _ _
#		 __  _(_)_ __ (_) |_ _ __ ___
#		 \ \/ / | '_ \| | __| '__/ __|
#		  >  <| | | | | | |_| | | (__
#		 /_/\_\_|_| |_|_|\__|_|  \___|

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
source ~/.xprofile &

# sync primary clipboard and GTK clipboard
autocutsel -fork &
autocutsel -selection PRIMARY -fork &

export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

bspwm

