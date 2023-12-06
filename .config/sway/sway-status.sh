#!/usr/bin/env dash

# Date
date=$(date "+%a %F %R")

# Alsa master volume
volume="$(amixer get Master | grep "Right:" | cut -f 7,8 -d " ")"

# Status bar
echo $volume "|" $date
