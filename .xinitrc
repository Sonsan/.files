#!/usr/bin/env sh
source ~/.profile
export XIM=nabi
export XIM_ARGS=
export XIM_PROGRAM="nabi"
export XMODIFIERS="@im=nabi"
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim

xrdb -merge ~/.Xresources

feh --bg-scale $WALLPAPER_1
picom -b      # compositor
dunst &       # notification deamon
nabi &        # Hangul switcher
pactl load-module module-loopback &  # So I can hear line-in audio (Nintendo Switch)

exec $WM
