#!/usr/bin/env bash

# this script downloads a bunch of tile gifs mainly for wallpaper usage
# usage example: hsetroot -tile pattern.png

url="http://web.archive.org/web/20130723184255/http://www.squidfingers.com/files/patterns/pattern_"

echo -n "download tile gifs: "

for i in {001..158}
do
    ( wget -q "${url}$i.gif" ) &
    if ! (($((10#$i + 1)) % 20)); then
        wait && echo -n "###"
fi
done

wait && echo " finished"

# i like png better
echo "convert *.gif to *.png"
find . -name '*.gif' | parallel --will-cite -j 8 convert '{}' '{.}.png'

echo "remove *.gif"
rm ./*.gif
