#!/bin/bash


function notify {
   notify-send -t 50000 'Warning!' "$1" --icon=dialog-warning
   echo $1 | mutt -s "Monitoring Notification" fabianbeuke@gmail.com
}

while :
do

# start after 5 min uptime
sleep 5m

# make sure we have internet connection
if ping -c 1 google.com > /dev/null; then

   # check if my homepage is online
   if !(curl https://beuke.org | grep -q Fabian); then
      notify 'beuke.org offline'
   fi

   # check if autoupdates stopped
   if [[ $(checkupdates | wc -l) -gt 50 ]]; then
      notify 'autoupdates stopped'
   fi

   # check if my aur packages source is avaiable
   if [[ !(`wget -S --spider http://mirror.easyname.at/blackarch/blackarch/os/x86_64/dripcap-0.3.10-1-x86_64.pkg.tar.xz  2>&1 |  grep 'HTTP/1.1 200 OK'`) ]]; then
      notify 'dripcap offline/updated'
   fi

fi

# repeat every 5 hours
sleep 5h
done
