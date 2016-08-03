while sleep 60;
do
away=$(echo -e "scale=0;$(xprintidle)/1000/60"|bc)

# if afk time longer than 10 min 
if  [ "10" -lt "$away" ] && 
    # and sound is not running
    [ "0" -eq $(pacmd list-sink-inputs | grep -c 'state: RUNNING') ] && 
    # sound is not mute
    ! pulseaudio-ctl | grep -q yes;
then
    #amixer set Master toggle; 
    pulseaudio-ctl mute;
fi
done
