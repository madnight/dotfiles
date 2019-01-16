awk '/cpu/ {usage=($2+$4)*100/($2+$4+$5)} END {printf "%0.0f%s\n", usage, "%"}' /proc/stat
