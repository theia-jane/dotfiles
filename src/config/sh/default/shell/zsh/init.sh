autoload -U compinit
compinit

bindkey "\C-f" history-incremental-search-forward 
bindkey "\C-r" history-incremental-search-backward

bindkey "\C-n" down-line-or-history 
bindkey "\C-p" up-line-or-history 

autoload -U edit-command-line
zle -N edit-command-line
bindkey "\C-x\C-e" edit-command-line 

HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history
setopt APPEND_HISTORY # Don't erase history
setopt EXTENDED_HISTORY # Add additional data to history like timestamp
setopt INC_APPEND_HISTORY # Add immediately
setopt HIST_SAVE_NO_DUPS # Don't save any duplicates
setopt NO_HIST_BEEP # Don't beep
setopt SHARE_HISTORY # Share history between session/terminals

SCRIPT_DIR="$(get_script_dir "$0")"
source "${SCRIPT_DIR}/plugins.sh"
