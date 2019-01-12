# wallpaper
# WP=$HOME/Pictures/xubuntu-trusty.png
# set wallpaper on three monitors
# feh --bg-scale $WP $WP $WP

declare -i last_called=0
declare -i throttle_by=10

@throttle() {
  local -i now=$(date +%s)
  if (($now - $last_called >= $throttle_by))
  then
    "$@"
  fi
  last_called=$(date +%s)
}

dock() {
    bash $HOME/.screenlayout/thinkdock.sh
    sleep 5

    for monitor in $(bspc query -M); do
        bspc monitor $monitor -d ""
    done

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
}

thinkpad() {
    for monitor in $(bspc query -M); do
        bspc monitor $monitor -d ""
    done

    sh $HOME/.screenlayout/thinkpadlcd.sh
    sleep 5

    PRIMARY=$(xrandr | grep \ connected | grep LVDS1 | awk {'print $1'})
    bspc monitor $PRIMARY -d 0 1 2 3 4 5 6 7 8 9
    bspc config top_padding 22

    setxkbmap de
    killall polybar || true
    while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done
    export MONITOR=$PRIMARY && SHELL=/bin/bash polybar -r top-bspwm-thinkpad &
}

bspc subscribe monitor | while read -r line; do
  case $line in
      monitor_geometry*)

        if [ "$(xrandr | grep \ connected | wc -l)" -eq "4" ]; then
          @throttle dock
        else
          @throttle thinkpad
        fi

      ;;
      *)
      ;;
  esac
done
