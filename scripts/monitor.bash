while sleep 10s
do
i=1
for monitor in $(bspc query -M); do
    bspc monitor $monitor -d {$i..$(($i+5))}
    let "i += 6"
done
mons -e right > /dev/null
done
