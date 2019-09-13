#export PATH="$PATH:$HOME/.local/bin"

#export PATH="$PATH:$HOME/.local/bin"
export PATH="$(du $HOME/.local/bin/ | cut -f2 | tr '\n' ':')$PATH"
export EDITOR="emacs"
export BROWSER="qutebrowser"
export BIB="$HOME/Documents/University/LaTeX/uni.bib"


# set bashrc path
echo "$0" | grep "bash$" >/dev/null && [ -f ~/.bashrc ] && source "$HOME/.bashrc"

