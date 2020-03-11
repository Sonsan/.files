#Path
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

# Programs
export TERMINAL="st"
export EDITOR="vim"
export BROWSER="firefox"
export READER="zathura"

# Directories
export WALLPAPER_DIR=~/Pictures/Wallpapers

# Files
export BIB=~/Documents/University/LaTeX/uni.bib
export WALLPAPER_1=$WALLPAPER_DIR/Space/Fictional/SpaceDog.jpg
export WALLPAPER_2=$WALLPAPER_DIR/MISC/vim.jpg

# Window Manager
export WM="i3"
export STATUSBAR="i3blocks"

# MISC
export LOCK="betterlockscreen -l"
export LOCATION="Frankfurt"
export NAME="Nils Sterz"
