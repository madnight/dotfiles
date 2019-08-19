#!/usr/bin/env bash

icon="$HOME/scripts/lock.png"
tmpbg='/tmp/screen.png'

(( $# )) && { icon=$1; }

rm "$tmpbg"
scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"

OFFSET_X=$(identify $icon | awk '{ print $3 }' | awk -F'x' '{print $1}')
OFFSET_Y=$(identify $icon | awk '{ print $3 }' | awk -F'x' '{print $2}')
mon=$(xrandr | grep primary | grep connected | grep -v disconnected | sed s/primary// | awk '{ print $3 }' )
X=$(echo $mon | awk -F'+' '{print $1, $2, $3}' | awk -F'x' '{print $1, $2, $3}' | awk '{print (($1/2)+$3)}')
Y=$(echo $mon | awk -F'+' '{print $1, $2, $3}' | awk -F'x' '{print $1, $2, $3}' | awk '{print ($2/2)+$4}')
convert "$tmpbg" "$icon" -geometry +$(($X-($OFFSET_X/2)))+$(($Y-($OFFSET_Y/2))) -composite -matte "$tmpbg"

i3lock -i "$tmpbg"
