autoload -U colors && colors
PS1='%(?.%F{green}∫.%F{160}?%?)%f %B%F{208}%1~%f%b %# '

[ -f "$HOME/.aliasrc" ] && source "$HOME/.aliasrc"

# HISTORY
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.cache/zsh/history


# COMPLETIONS
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zmodload zsh/complist
compinit

# Include hidden files in autocomplete:
_comp_options+=(globdots)

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats '%F{32}(%b)%f'
zstyle ':vcs_info:*' enable git

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND="fd --type f --hidden --follow --exclude .git"

# PLUGINS
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh 2>/dev/null

