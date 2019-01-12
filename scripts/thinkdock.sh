bspc config remove_disabled_monitors true

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

cleanup() {
    for monitor in $(bspc query -M); do
        bspc monitor $monitor -d ""
    done
    killall polybar > /dev/null
}

reinit() {
    setxkbmap de
    WP=$HOME/Pictures/hollywood.jpg
    feh --bg-scale $WP $WP $WP
    export MONITOR=$PRIMARY && SHELL=/bin/bash polybar -r top-bspwm-thinkpad &
}

dock() {
    cleanup

    bash $HOME/.screenlayout/thinkdock.sh
    sleep 5

    bspc monitor LVDS1 --remove
    bspc wm --adopt-orphans

    PRIMARY=$(xrandr | grep \ connected | grep 1920x1080 | awk {'print $1'})
    LEFT=$(xrandr | grep \ connected | grep 1680x1050+0 | awk {'print $1'})
    RIGHT=$(xrandr | grep \ connected | grep VGA | awk {'print $1'})

    bspc config top_padding    0

    bspc config -m $PRIMARY window_gap     10
    bspc config -m $PRIMARY right_padding  8
    bspc config -m $PRIMARY left_padding   8
    bspc config -m $PRIMARY bottom_padding 12
    bspc config -m $PRIMARY top_padding 38

    bspc monitor $LEFT -d 1 2 3
    bspc monitor $PRIMARY -d 4 5 6
    bspc monitor $RIGHT -d 7 8 9

    reinit
}

thinkpad() {
    cleanup

    sh $HOME/.screenlayout/thinkpadlcd.sh
    sleep 5

    for monitor in $(bspc wm -d | jq -r '.monitors[] | .name' | grep -v LVDS1); do
        bspc monitor $monitor --remove
    done

    PRIMARY=$(xrandr | grep \ connected | grep LVDS1 | awk {'print $1'})

    bspc monitor -d 1 2 3 4 5 6 7 8 9
    bspc wm --adopt-orphans

    bspc config top_padding   22
    bspc config window_gap     0
    bspc config right_padding  0
    bspc config left_padding   0
    bspc config bottom_padding 0

    reinit
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
