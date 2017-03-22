#!/bin/bash

function notify {
   notify-send -t 5s 'Warning!' "$1" --icon=dialog-warning
   #echo "$1" | mutt -s "Monitoring Notification" fabianbeuke@gmail.com
   echo "" | mutt -s "$1" fabianbeuke@gmail.com
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
   ! curl --silent --head --fail  https://github.com/dripcap/dripcap/releases/download/v0.6.4/dripcap-linux-amd64.deb | grep 'Found' > /dev/null && \
      notify 'dripcap offline/updated'

fi

# repeat every 5 hours
sleep 8h
done
