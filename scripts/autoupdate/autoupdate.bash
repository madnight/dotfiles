#!/bin/bash

while sleep 30m
do

# Get News from Archlinux.org
news=$(python /home/x/scripts/archnews -d 8)

# inter := interaction, intervention; requi := requires, required
if [[ "$news" == *inter* || "$news" == *requi*  || "$news" == *manual* ]]; then
    echo "!!! Critical UPDATE on archlinux.org found !!!";
else
    # Check and install Pacman Update
    if [ ! "$(checkupdates | wc -l)" == 0  ]; then
        pacman -Syu --noconfirm
        # download kernel and headers /wo install <- later on shutdown
        pacman -Sw linux linux-headers
    fi

    # Delete orphran packages and clean paccache
    if [ ! "$(pkg-list_true_orphans | wc -l)" == 0  ];then
        pacman -Rns --noconfirm $(pkg-list_true_orphans)
        # remove pacman pkg cache exept the latest one
        paccache -r -k 1
    fi
fi

done
