#!/usr/bin/env sh
source ~/.profile
xrdb -merge ~/.Xresources

setxkbmap -layout de

feh --bg-scale $WALLPAPER_1
picom -b      # compositor
dunst &       # notification deamon

exec $WM
