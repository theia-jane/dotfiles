export notes="${HOME}/notes"
export cfg_sh="${cfg}/sh"
export docs="${HOME}/Documents"
export dbox="${HOME}/Nextcloud"
export org="${HOME}/org"

export p="${HOME}/Projects"
export dot="$p/dotfiles"
export DOOMDIR="${cfg}/doom"
export scripts="$dot/src/scripts"
export s=$scripts
export CLICOLOR=1
export GTAGSLABEL="pygments"

if [ ! -z "$(which rustc)" ]; then
  # rustup component add rust-src
  export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
fi

if [ ! -z "$(which emacsdclient)" ]; then
  EDITOR="emacsdclient -c"
fi

if [ ! -z "$(which nvim)" ]; then
  EDITOR="${EDITOR:-nvim}"
  export MANPAGER="nvim -c 'set ft=man' -"
fi

test -r /home/tware/.opam/opam-init/init.sh && . /home/tware/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

export EDITOR="${EDITOR:-vim}"
