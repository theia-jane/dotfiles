export notes="${HOME}/notes"
export cfg_sh="${cfg}/sh"
export docs="${HOME}/Documents"
export dbox="${HOME}/Nextcloud"
export org="${HOME}/org"
export p="${HOME}/Projects"
export dot="$p/dotfiles"
export scripts="$dot/src/scripts"
export s=$scripts
export CLICOLOR=1
export GTAGSLABEL="pygments"


if [ "$(which nvim)" ]; then
  export EDITOR=nvim
  export MANPAGER="nvim -c 'set ft=man' -"
fi
