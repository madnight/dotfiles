urxvt -e zsh -c "bspc node -t floating && vim ~/notes " &

sleep 1s && \
  wmctrl -i -r $(xdotool search --onlyvisible --name notes) -e 0,350,0,1220,400

