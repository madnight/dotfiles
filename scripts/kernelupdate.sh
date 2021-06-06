!/bin/bash

while sleep 10m
do
    if [[ "$(pacman -Q linux | cut -d " " -f 2)" != *"$(uname -r | cut -d- -f2)"* ]]; then
        if [[ "$(pacman -Q linux-ls | cut -d " " -f 2)" != *"$(uname -r | cut -d- -f2)"* ]]; then
            notify-send "Kernel Update!" "Reboot required!"
        fi
    fi
done
