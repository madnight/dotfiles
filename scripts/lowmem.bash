#!/bin/bash

#Minimum available memory limit, MB
THRESHOLD=2000

#Check time interval, sec
INTERVAL=30

while :
do

    free=$(free -m|awk '/^Mem:/{print $4}')
    buffers=$(free -m|awk '/^Mem:/{print $6}')
    cached=$(free -m|awk '/^Mem:/{print $7}')
    available=$(free -m | sed -n 2p | awk '{print $4}')

    message="Free $free""MB"", buffers $buffers""MB"", cached $cached""MB"", available $available""MB"""

    if [ $available -lt "$THRESHOLD" ]
        then
        notify-send "Memory is running out!" "$message"
    fi

    echo $message
    sleep $INTERVAL
done