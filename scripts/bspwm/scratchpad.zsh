if xdotool search --name notes &> /dev/null
then
  bspc node $(bspc query -N -d -n .hidden | head -n 1) -g hidden || bspc node --flag hidden
  wmctrl -a notes
else
  urxvt -e zsh -c "bspc node -t floating && vim ~/notes " &
  sleep 1s && \
    wmctrl -i -r $(xdotool search --name notes) -e 0,350,0,1220,400
fi

