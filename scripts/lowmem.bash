#!/bin/bash

#Minimum available memory limit, MB
THRESHOLD=2000

#Check time interval, sec
INTERVAL=30

while :
do
    total=$(free -m|awk '/^Mem:/{print $2}')
    used=$(free -m|awk '/^Mem:/{print $3}')
    available=$(free -m|awk '/^Mem:/{print $7}')

    message="total $total""MB"", used $used""MB"", available $available""MB"""

    if [ $available -lt "$THRESHOLD" ]
        then
        notify-send "Memory is running out!" "$message"
    fi

    echo $message
    sleep $INTERVAL
done
