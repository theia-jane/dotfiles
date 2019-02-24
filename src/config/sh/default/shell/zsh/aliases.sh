alias .so=". ~/.zshrc"
alias bindkey.f="bindkey | fzf > /dev/null"

alias -g filter.dir="grep -Ev '\\/{0,1}(\\.git|node_modules|.idea)\\/{0,1}'"
alias -g filter.empty="grep ."

alias -g WDO="| while read -r line; do;"
alias -g G='| grep'
alias -g L='| less'
alias -g F='| fzf'
alias -g FM='| fzf -m'
alias -g A='| awk'
