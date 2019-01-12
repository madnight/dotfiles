#!/bin/sh
xrandr               \
--output LVDS1 --off \
--output HDMI3 --off \
--output HDMI2 --off \
--output HDMI1 --off \
--output VGA1  --off

xrandr --output HDMI3 --mode 1680x1050 --pos 0x30 --rotate normal
xrandr --output VGA1 --mode 1680x1050 --pos 3600x0 --rotate normal
sleep 2
xrandr --output HDMI2 --primary --mode 1920x1080 --pos 1680x0 --rotate normal

setxkbmap de
