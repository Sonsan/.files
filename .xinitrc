setxkbmap -layout de

feh --bg-scale ${WALLPAPER_1}

dunst &         # nofification manager
picom -b      # compositor

exec ${WM}
