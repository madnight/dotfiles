How to restore the arch sys
===========================

Packages
--------

/etc/pacman.conf  
[archlinuxfr]  
 SigLevel = Never  
 Server = http://repo.archlinux.fr/$arch  

[infinality-bundle]  
 SigLevel = Never  
 Server = http://bohoomil.com/repo/$arch  

In such a case, you may still want to install all available packages from that list:  
pacman -S --needed $(comm -12 <(pacman -Slq|sort) <(sort badpkdlist) )  

You may also try to install all unavailable packages (those not in the repos) from the AUR using yaourt:  
yaourt -S --needed $(comm -13 <(pacman -Slq|sort) <(sort badpkdlist) )


https://wiki.archlinux.org/index.php/Pacman_tips#Backing_up_and_retrieving_a_list_of_installed_packages

fstab
-----
/dev/sda3 /home/datadisk ntfs defaults,uid=1000,gid=1000,dmask=027,fmask=137,rw 0 0

Network
-------
With WM:  
systemctl enable NetworkManager.service

On Console:  
dhcpcd

Auto Login
----------

sudo systemctl edit getty@tty1 and pasting its content:  

[Service]  
ExecStart=  
ExecStart=-/usr/bin/agetty --autologin username --noclear %I 38400 linux  

Bootable USB
----------

Run the following command, replacing /dev/sdx with your drive, e.g. /dev/sdb. (do not append a partition number, so do not use something like /dev/sdb1):

sudo umount /dev/sdx && dd bs=4M if=/path/to/archlinux.iso of=/dev/sdx status=progress && sync
