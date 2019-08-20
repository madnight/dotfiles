#!/usr/bin/env bash
icon="$HOME/scripts/lock.png"

# takes about 0.37 sec
tmpbg=$(/home/x/git/i3lock-fancy-rapid/i3lock-fancy-rapid 5 "pixel")

(( $# )) && { icon=$1; }

# this is fast
OFFSET_X=$(identify $icon | awk '{ print $3 }' | awk -F'x' '{print $1}')
OFFSET_Y=$(identify $icon | awk '{ print $3 }' | awk -F'x' '{print $2}')

# xrandr cache
if [ ! -f /tmp/xrandr_cache ]; then
    xrandr | grep primary | grep connected | grep -v disconnected | sed s/primary// | awk '{ print $3 }' > /tmp/xrandr_cache
fi
mon=$(cat /tmp/xrandr_cache)

# this is fast
X=$(echo $mon | awk -F'+' '{print $1, $2, $3}' | awk -F'x' '{print $1, $2, $3}' | awk '{print (($1/2)+$3)}')
Y=$(echo $mon | awk -F'+' '{print $1, $2, $3}' | awk -F'x' '{print $1, $2, $3}' | awk '{print ($2/2)+$4}')
dims=$(identify "$tmpbg" | awk '{print $3}')

# gm composite is twice as fast as convert + direct raw pipe to i3lock
gm composite -geometry +$(($X-($OFFSET_X/2)))+$(($Y-($OFFSET_Y/2))) "$icon" "$tmpbg" RGB:- | \
    i3lock --raw "$dims:rgb" --image /dev/stdin
