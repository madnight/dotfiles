#!/bin/sh
conky  -c /home/x/.i3/conkybar.conf | /home/x/.i3/i3-wsbar -c "dzen2 -x %x -dock -y 0 -x 0 -h 16 -w 1920 -h 16 -fn -*-fixed-medium-*-*-*-15-*-*-*-*-*-*-* -ta l -bg #181512"          
