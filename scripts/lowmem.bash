#!/bin/bash

# Minimum available memory limit, MB
memory=2000

while sleep 1s
do
    total=$(free -m|awk '/^Mem:/{print $2}')
    used=$(free -m|awk '/^Mem:/{print $3}')
    available=$(free -m|awk '/^Mem:/{print $7}')
    message="total $total""MB"", used $used""MB"", available $available""MB"""

    [ "$available" -lt "$memory" ] && \
    notify-send "Memory is running out!" "$message"
done

free <- readProcess "free" ["-m"] []
words free
let get x = read $ words free !! x :: Int
let [total, used, avail] = get <$> [7, 8, 12]

let warning = ("'Memory is running out " ++ (show avail) ++ " MB " ++ "left'")
system ("notify-send " ++ warning)
when (avail < 2000) []
