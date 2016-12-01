#!/bin/bash

function notify {
   notify-send -t 5s 'Warning!' "$1" --icon=dialog-warning
   echo "$1" | mutt -s "Monitoring Notification" fabianbeuke@gmail.com
}

while sleep 5m
do

# make sure we have internet connection
if ping -c 1 google.com > /dev/null; then

   # check if my homepage is online
   ! (curl https://beuke.org | grep -q Fabian) && \
      notify 'beuke.org offline'

   # check if autoupdates stopped
   [ "$(checkupdates | wc -l)" -gt 50 ] && \
      notify 'autoupdates stopped'

   # check if my aur packages source is avaiable
   ! wget -S --spider http://mirror.easyname.at/blackarch/blackarch/os/x86_64/dripcap-0.3.10-1-x86_64.pkg.tar.xz  2>&1 | grep 'HTTP/1.1 200 OK' && \
      notify 'dripcap offline/updated'

fi

# repeat every 5 hours
sleep 5h
done
