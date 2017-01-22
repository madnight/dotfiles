#!/bin/sh
#                               _|            _|    _|                          
#                     _|    _|      _|_|_|        _|_|_|_|  _|  _|_|    _|_|_|  
#                       _|_|    _|  _|    _|  _|    _|      _|_|      _|        
#                     _|    _|  _|  _|    _|  _|    _|      _|        _|        
#                     _|    _|  _|  _|    _|  _|      _|_|  _|          _|_|_|  
                                                                               
# wallpaper 
hsetroot -solid '#1c1c1c'

xset r rate 150 50  # set your speed!
xset -dpms          # no screensaver
xset s off 
xset s off -dpms
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b             # disable terminal bell

xsetroot -cursor_name left_ptr &

xrdb -merge ~/.Xresources 
xrdb -merge ~/.Xdefaults

# load key bindings
xbindkeys

# load nvidia-settings (force low gpu settings non gaming)
nvidia-settings -l

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
#exec i3
startxfce4
