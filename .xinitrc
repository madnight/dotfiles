#!/bin/sh
#                               _|            _|    _|                          
#                     _|    _|      _|_|_|        _|_|_|_|  _|  _|_|    _|_|_|  
#                       _|_|    _|  _|    _|  _|    _|      _|_|      _|        
#                     _|    _|  _|  _|    _|  _|    _|      _|        _|        
#                     _|    _|  _|  _|    _|  _|      _|_|  _|          _|_|_|  
                                                                               
                                                                               
hsetroot -solid '#1c1c1c'
#hsetroot -tile ~/Pictures/tile1.jpg 

xset -dpms 
xset s off 
xset +fp ~/.fonts
xset +fp /usr/share/fonts/local
xset fp rehash
xset -b # disable terminal bell
xsetroot -cursor_name left_ptr &
xrdb -merge ~/.Xresources 
xrdb -merge ~/.Xdefaults

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#exec i3 exec cinnamon-session
startxfce4
