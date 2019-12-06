# Path
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"

# Programs
export TERMINAL_EMULATOR="termite"
export TERMINAL="${TERMINAL_EMULATOR} -e"
export EDITOR="emacs"
export BROWSER="qutebrowser"
export VIEWER="zathura"


# Files
export BIB="$HOME/Documents/University/LaTeX/uni.bib"
export WALLPAPER=~/Pictures/Wallpapers/Touhou_Christmas.png

# Window Manager
export WM="i3"
export STATUSBAR="polybar"
