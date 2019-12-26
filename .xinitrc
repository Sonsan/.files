source ~/.profile
setxkbmap -layout de

feh --bg-scale $WALLPAPER_1
picom -b      # compositor

exec $WM
