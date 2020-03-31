#!/usr/bin/env sh
source ~/.profile
export XIM=nabi
export XIM_ARGS=
export XIM_PROGRAM="nabi"
export XMODIFIERS="@im=nabi"
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim

xrdb -merge ~/.Xresources

xwallpaper --center "$HOME/Pictures/Wallpaper/Space/Fictional/SpaceDog.jpg"
picom -b      # compositor
dunst &       # notification deamon
nabi &        # Hangul switcher
nm-applet &
# pactl load-module module-loopback &  # So I can hear line-in audio (Nintendo Switch)

while xsetroot -name "🔊 `pamixer --get-volume-human` | `date +"%m/%d %H:%M"`"; do
    sleep 1
done &

exec $WM
