export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/nils/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

ENABLE_CORRECTION="false"  # i'm using thefuck

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Makes repository check faster
DISABLE_UNTRACKED_FILES_DIRTY="true"

# HIST_STAMPS="mm/dd/yyyy"

plugins=(colored-man-pages docker zsh-autosuggestions zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh


# fzf stuff
export FZF_DEFAULT_COMMAND='find -L'

# User configuration
eval $(thefuck --alias)
source $HOME/.aliasrc

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
