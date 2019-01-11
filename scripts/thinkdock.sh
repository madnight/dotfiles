# wallpaper
# WP=$HOME/Pictures/xubuntu-trusty.png
# set wallpaper on three monitors
# feh --bg-scale $WP $WP $WP

bspc subscribe monitor | while read -r line; do
  case $line in
      monitor_add*)

        if [ "$(xrandr | grep \ connected | wc -l)" -eq "4" ]; then

          for monitor in $(bspc query -M); do
              bspc monitor $monitor -d ""
          done

          bash $HOME/.screenlayout/thinkdock.sh
          sleep 5

          PRIMARY=$(xrandr | grep \ connected | grep 1920x1080 | awk {'print $1'})
          LEFT=$(xrandr | grep \ connected | grep 1680x1050+0 | awk {'print $1'})
          RIGHT=$(xrandr | grep \ connected | grep VGA | awk {'print $1'})

          bspc config -m $PRIMARY top_padding 25
          bspc monitor $LEFT -d 1 2 3
          bspc monitor $PRIMARY -d 4 5 6
          bspc monitor $RIGHT -d 7 8 9

          setxkbmap de
          killall polybar || true
          while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
          export MONITOR=$PRIMARY && SHELL=/bin/bash polybar -r top-bspwm-thinkpad &
        fi

        if [ "$(xrandr | grep \ connected | wc -l)" -eq "1" ]; then

          for monitor in $(bspc query -M); do
              bspc monitor $monitor -d ""
          done

          sh $HOME/.screenlayout/thinkpadlcd.sh
          sleep 5

          PRIMARY=$(xrandr | grep \ connected | awk {'print $1'})
          bspc monitor $PRIMARY -d 0 1 2 3 4 5 6 7 8 9
          bspc config top_padding 22

          setxkbmap de
          killall polybar || true
          while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
          export MONITOR=$PRIMARY && SHELL=/bin/bash polybar -r top-bspwm-thinkpad &
        fi

               ;;
      *)
      ;;
  esac
done
