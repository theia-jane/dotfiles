export docs=~/Documents
export p=~/Projects
export dbox=~/Nextcloud
export org=$HOME/org
export scripts=$cfg/scripts
export s=$scripts
export CLICOLOR=1

if [ "$(which nvim)" ]; then
  export EDITOR=nvim
  export MANPAGER="nvim -c 'set ft=man' -"
fi

PATH=""
add_path "/usr/local/bin"
add_path "/usr/bin"
add_path "/bin"
add_path "/usr/sbin"
add_path "/sbin"
add_path "${HOME}/miniconda3/bin"
add_path "${HOME}/bin"
add_path "${scripts}/utils"
add_path "${HOME}/go/bin"
add_path "${HOME}/node_tools/node_modules/.bin" 
add_path "${HOME}/.cargo/bin" 

export GOBIN="${HOME}/go/bin"
add_path "${GOBIN}"

export N_PREFIX="$HOME/n"; 
add_path "$N_PREFIX/bin"

export PATH
