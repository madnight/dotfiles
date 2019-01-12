#!/bin/sh
# xrandr               \
# --output LVDS1 --off \
# --output HDMI3 --off \
# --output HDMI2 --off \
# --output HDMI1 --off \
# --output VGA1  --off

xrandr --output LVDS1 --primary --mode 1600x900 --pos 0x0 --rotate normal

setxkbmap de
