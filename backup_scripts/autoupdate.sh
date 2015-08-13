#!/bin/sh

while :
do
sleep 1800

# Get News from Archlinux.org
news=$(python /home/x/scripts/archnews -d 8)

# inter <=> interaction, intervention  // requi <=> requires, required
if [[ $news == *inter* || $news == *requi*  || $news == *manual*   ]]
then
        echo "!!! Critical UPDATE on archlinux.org found !!!";
else

# Check and install Pacman Update
if [ ! $(checkupdates | wc -l) == 0  ];then
	pacman -Syu --noconfirm
fi

# Temporarly disabled no root yaourt possible anymore
# Check and install AUR Update ( 2>/dev/null <=> suppress warnings )
#if [ ! $(yaourt -Qua 2>/dev/null | wc -l) == 0  ];then
#	yaourt -Syua --noconfirm --devel
#fi

# Delete orphran packages and clean paccache
if [ ! $(pkg-list_true_orphans | wc -l) == 0  ];then
	pacman -Rns --noconfirm $(pkg-list_true_orphans)
	paccache -r
fi

fi
done
