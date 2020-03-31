#Path
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

# Programs
export TERMINAL="st"
export EDITOR="vim"
export BROWSER="firefox"
export READER="zathura"

# Directories
export WALLPAPER_DIR=~/Pictures/Wallpaper

# Files
export BIB=~/Documents/University/LaTeX/uni.bib
export WALLPAPER_1=$WALLPAPER_DIR/Space/Fictional/SpaceDog.jpg
export WALLPAPER_2=$WALLPAPER_DIR/MISC/vim.jpg

# Window Manager
export WM="dwm"
export STATUSBAR="dwmblocks"

# MISC
export LOCK="slock"
export LOCATION="Frankfurt"
export NAME="Nils Sterz"

# ~/ Clean-up:
export PASSWORD_STORE_DIR="$HOME/.local/share/password-store"
export GTK2_RC_FILES="$HOME/.config/gtk-2.0/gtkrc-2.0"
export NOTMUCH_CONFIG="$HOME/.config/notmuch-config"
export LESSHISTFILE="-"

[ "$(tty)" = "/dev/tty1" ] && ! pgrep -x Xorg >/dev/null && exec startx
