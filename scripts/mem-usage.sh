awk '/MemFree/ { printf "%.1f GB\n", $2/1024/1024 }' /proc/meminfo
