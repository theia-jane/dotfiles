[ ! -f ~/.antigen.zsh ] && curl -L git.io/antigen > ~/.antigen.zsh
source ~/.antigen.zsh
antigen bundle cusxio/delta-prompt > /dev/null
antigen bundle zsh-users/zsh-syntax-highlighting > /dev/null 
antigen apply
