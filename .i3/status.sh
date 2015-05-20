#!/bin/sh

# ------------------------------------------------------
# file:     $HOME/.scripts/dzenstat.sh
# author:   Ramon Solis - http://cyb3rpunk.wordpress.com
# modified: June 2011
# vim:fenc=utf-8:nu:ai:si:et:ts=4:sw=4:ft=sh:
# ------------------------------------------------------

# -------------------------
# Dzen settings & Variables
# -------------------------
ICONPATH="/home/x/scripts/dzenicons/"
COLOR_ICON="#ffffff"
CRIT_COLOR="#ff2c4a"
DZEN_FG="#333333"
DZEN_BG="#000000"
HEIGHT=12
WIDTH=470
#RESOLUTIONW=`xrandr | grep -r "current" | awk '{print $8}'` 
RESOLUTIONW=1920
RESOLUTIONH=786
X=$(($RESOLUTIONW-$WIDTH))
Y=$(($RESOLUTIONH-$HEIGHT-1))
BAR_FG="#ffffff"
BAR_BG="#808080"
BAR_H=10
BAR_W=60
FONT="-*-fixed-medium-*-*-*-15-*-*-*-*-*-*-*"
#FONT="-artwiz-anorexia-medium-r-normal--11-110-75-75-p-90-iso8859-1"
SLEEP=1
VUP="amixer -c0 -q set Master 4dB+"
VDOWN="amixer -c0 -q set Master 4dB-"
EVENT="button3=exit;button4=exec:$VUP;button5=exec:$VDOWN"
#DZEN="dzen2 -x $X -y $Y -w $WIDTH -h $HEIGHT -fn $FONT -ta 'c' -bg $DZEN_BG -fg $DZEN_FG -e "button3=exit;button4=exec:$VUP;button5=exec:$VDOWN""
#DZEN==`perl i3-wsbar -c "dzen2 -x %x -ta r -y 1066 -h 16 -w 1920 -fn $FONT -fg #CCCCCC -bg #181512  -ta c -dock"`
DZEN="dzen2 -ta r -y 1066 -x 0 -h 16 -w 1920 -fn $FONT -fg #CCCCCC -bg #181512  -ta c -dock"
#DZEN="bar"

# ---------
# Infinite loop
# -------------
while :; do
  sleep ${SLEEP}

  # ---------
  # Functions
  # ---------

  Rss ()
  {
    RSS=$(rsstail -u http://mix.chimpfeedr.com/6356e-abcd -1 | head -n 1 | cut -c8-)
    echo -n "^fg($COLOR_ICON)^i($ICONPATH/info01.xbm)^fg(#FFFFFF) ${RSS}"
    return
  }

  Vol ()
  {
    ONF=$(amixer get Master | awk '/Front\ Left:/ {print $7}' | tr -d '[]')
    VOL=$(amixer get Master | awk '/Front\ Left:/ {print $5}' | tr -d '[]%')
    if [[ ${ONF} == 'off' ]] ; then
      echo -n "^fg($CRIT_COLOR)^i($ICONPATH/spkr_01.xbm)^fg()" $(echo "0"  | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BAR_W -s o -ss 1 -sw 2 -nonl)
    else
      echo -n "^fg($COLOR_ICON)^i($ICONPATH/spkr_01.xbm)^fg()" ${VOL} $(echo $VOL | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BAR_W -s o -ss 1 -sw 2 -nonl)
    fi
    return
  }

  Mem ()
  {
    MEM=$(free -m | grep '-' | awk '{print $3}')
    echo -n "^fg($COLOR_ICON)^i($ICONPATH/mem.xbm)^fg() ${MEM} M"
    return
  }

  Mail ()
  {
    MAIL=$(perl /home/x/scripts/mail.pl | tac | head -n 1)  
    echo -n " ^fg($COLOR_ICON)^i($ICONPATH/mail.xbm)^fg() ${MAIL} "
  }

  Temp ()
  {
    TEMP=$(acpi -t | awk '{print $4}' | tr -d '.0')
    if [[ ${TEMP} -gt 63 ]] ; then
      echo -n "^fg($CRIT_COLOR)^i($ICONPATH/temp.xbm)^fg($CRIT_COLOR) ${TEMP}°C" $(echo ${TEMP} | gdbar -fg $CRIT_COLOR -bg $BAR_BG -h $BAR_H -s v -sw 5 -ss 0 -sh 1 -nonl)
    else 
      echo -n "^fg($COLOR_ICON)^i($ICONPATH/temp.xbm)^fg() ${TEMP}°C" $(echo ${TEMP} | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -s v -sw 5 -ss 0 -sh 1 -nonl)
    fi
    return
  }

  Disk ()
  {
    SDA2=$(df -h / | awk '/\/$/ {print $5}' | tr -d '%')
    SDA4=$(df -h /home | awk '/home/ {print $5}' | tr -d '%')
    if [[ ${SDA2} -gt 60 ]] ; then
      echo -n "^fg($COLOR_ICON)^i($ICONPATH/fs_01.xbm)^fg() /:${SDA2}% $(echo $SDA2 | gdbar -fg $CRIT_COLOR -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
    else
      echo -n "^fg($COLOR_ICON)^i($ICONPATH/fs_01.xbm)^fg() /:${SDA2}% $(echo $SDA2 | gdbar -fg $BAR_FG -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
    fi
    if [[ ${SDA4} -gt 80 ]] ; then
      echo -n "  ~:${SDA4}% $(echo $SDA4 | gdbar -fg $CRIT_COLOR -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
    else
      echo -n "  ~:${SDA4}% $(echo $SDA4 | gdbar -fg $BAR_FG -bg $BAR_BG -h 7 -w 40 -s o -ss 0 -sw 2 -nonl)"
    fi
    return
  }

  MPD ()
  {
    MPDPLAYING=$( mpc -f %title% | head -n 1)
    if [ -z "$MPDPLAYING" ]; then
        MPDPLAYING="BBC RADIO 1"
    fi 
    if [[ $MPDPLAYING == *"n/a"* ]]; then
        MPDPLAYING="Radio offline"
    fi 
     echo -n "^fg($COLOR_ICON)^i($ICONPATH/note.xbm)^fg() ${MPDPLAYING} "
    return
  }

  Date ()
  {
    TIME=$(date +%R)
    echo -n "^fg($COLOR_ICON)^i($ICONPATH/clock.xbm)^fg(#FFFFFF) ${TIME}"
    return
  }

  Between ()
  {
    #echo -n " ^fg(#7298a9)^r(2x2)^fg() "
    echo -n "  :::   "
    return
  }

  Temp ()
  {
    TEMP=$(grep "yweather:condition" ~/.cache/weather.xml | grep -o "temp=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*")
    WEATHER=$( grep "yweather:condition" ~/.cache/weather.xml | grep -o "text=\"[^\"]*\"" | grep -o "\"[^\"]*\"" | grep -o "[^\"]*"  )
    echo -n "^fg($COLOR_ICON)^i($ICONPATH/temp.xbm)^fg(#FFFFFF) ${TEMP}°C ${WEATHER} "
    return
  }

  OSLogo ()
  {
    OS=$(uname -a | awk '{print $2}')
    echo -n " ^fg($COLOR_ICON)^i($ICONPATH/${OS}.xbm)^fg()"
    return
  }
  # --------- End Of Functions

  # -----
  # print 
  # -----
  Print () {
    #	oSLogo
    #        Between
    Mail
    Between
    MPD
    Between
    Temp
 #   Between
#    Rss
    #        Between
    #        mem
    #        Between
    #        Vol
    #        Between
    #        Date
    echo
    return
  }

  echo "$(Print)" 
done | $DZEN &
