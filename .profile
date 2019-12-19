# Path
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

# Programs
export TERMINAL_EMULATOR="st"
export TERMINAL="${TERMINAL_EMULATOR} -e"
export EDITOR="emacs"
export BROWSER="firefox"
export VIEWER="zathura"

# Directories
export WALLPAPER_DIR=~/Pictures/Wallpapers

# Files
export BIB=~/Documents/University/LaTeX/uni.bib
export WALLPAPER_1=$WALLPAPER_DIR/Landscapes/Real/
export WALLPAPER_2=$WALLPAPER_DIR/Landscapes/Real/Snowy_Forest_Road_daytime2.jpg

# Window Manager
export WM="i3"
export STATUSBAR="polybar"

# MISC
export LOCATION="Floersheim"
