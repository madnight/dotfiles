#!/bin/bash

while sleep 5m
do

function update {

cd ~/Git/"$2" || exit

if [ "$1" == "svn" ]; then
    git svn rebase upstream/master
else
    git rebase -s recursive -X theirs upstream/master
fi

git push -f origin master
# anti spam prevention
sleep "$((RANDOM % 500))"
}

# make sure we have internet connection
if ping -c 1 google.com > /dev/null; then
  update "git" MetaGer
  update "git" scid
  update "git" sed
  update "git" gnupg
  update "git" grub
  update "git" nano

  update "svn" lfs
  update "svn" filezilla
  update "svn" gnuchess
  update "svn" valgrind
  update "svn" scidvspc
  update "svn" chessx
  update "svn" codeblocks

  update "svn" jdownloader/browser
  update "svn" jdownloader/trunk
  update "svn" jdownloader/MyJDownloaderClient
fi

# repeat every 2 hours
sleep 2h

done
