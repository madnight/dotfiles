top -bn1 | grep "Cpu" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{printf "%2.0f%\n",100 - $1"%"}'
