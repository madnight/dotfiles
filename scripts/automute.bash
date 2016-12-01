#!/bin/bash

while sleep 1m
do
    # if afk time longer than 10 min
    [ "10" -lt "$(echo -e "scale=0;$(xprintidle)/1000/60" | bc)" ] && \
    # and sound is not running
    [ "0" -eq "$(pacmd list-sink-inputs | grep -c 'state: RUNNING')" ] && \
    # sound is not mute
    ! pulseaudio-ctl | grep -q yes && \
    # amixer set Master toggle
    pulseaudio-ctl mute;
done
