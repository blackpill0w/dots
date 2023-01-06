#!/bin/bash

REAL_UID=$(id --real --user)
PID=$(pgrep --euid $REAL_UID gnome-session | head -n 1)
export DBUS_SESSION_BUS_ADDRESS=$(grep -z DBUS_SESSION_BUS_ADDRESS /proc/$PID/environ | cut -d= -f2- | sed -e "s/\x0//g")

DIR="/home/blackpill0w/Pictures/Wallpapers"
PIC=$(ls $DIR/* | shuf -n1)

gsettings set org.gnome.desktop.background picture-uri "file://$PIC"
gsettings set org.gnome.desktop.background picture-uri-dark "file://$PIC"
