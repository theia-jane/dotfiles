add_path() {
  if [ -d "$1" ]; then
    append_to_var PATH $1
  fi
}

append_to_var() {
  local var=$1
  local val=$2
  [ ! -z "$(eval echo "\$$var")" ] && eval $var="$(eval echo "\$$var"):"
  eval $var="$(eval echo "\$$var")${val}"
}

source_file() {
  if [ -f "$1" ]; then
    source "$1"
  fi
}

get_script_dir() {
  if [ "$SHELL_NAME" = "bash" ]; then
    SCRIPT_PATH="$(caller 0 | awk '{print $(NF)}')"
  else
    SCRIPT_PATH="$(echo "$funcstack[2]")"
  fi

  if [ "$OS" = "darwin" ]; then
    echo "$(dirname "$(stat -f "$SCRIPT_PATH")")" 
  else
    echo "$(dirname "$(readlink -f "$SCRIPT_PATH")")"
  fi
}

export SHELL_NAME="$(echo $0 | grep -oh '[a-z]*sh')"
export OS="$(uname | tr "[A-Z]" "[a-z]")"
export cfg="${HOME}/.config"
export cfg_sh="${cfg}/sh"
export docs="${HOME}/Documents"
export dbox="${HOME}/Nextcloud"
export org="${HOME}/org"
export p="${HOME}/Projects"
export dot="$p/dotfiles"
export scripts="$p/scripts"
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

PATH_DIR="$cfg/path"
if [ -d "${PATH_DIR}" ]; then
  for dir in "$(ls "${PATH_DIR}")"; do
		add_path "${PATH_DIR}/${dir}"
	done
fi

export PATH


if [ -d "${HOME}/perl5" ]; then
  PATH="${HOME}/perl5/bin${PATH:+:${PATH}}"; export PATH;
  PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
  PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
  PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
  PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;
fi

if [ -d "${HOME}/.rbenv" ]; then
  export PATH="${HOME}/.rbenv/bin:${PATH}"
  eval "$(rbenv init -)" 
fi

[ -d "${HOME}/.rbenv/plugins/ruby-build" ] && export PATH="${HOME}/.rbenv/plugins/ruby-builder/bin:${PATH}"

[ ! -d "${HOME}/.fzf" ] \
  && git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf 

[ ! -e "${HOME}/.fzf.${SHELL_NAME}" ] \
  && ~/.fzf/install

source ${HOME}/.fzf.${SHELL_NAME}

if [ ! -z "$(which ag)" ]; then
  export FZF_DEFAULT_COMMAND='(ag --hidden --ignore node_modules --ignore .git --ignore .idea --ignore .DS_Store -f -g "") 2> /dev/null'
fi

export FZF_DEFAULT_OPTS='--bind="ctrl-alt-a:select-all+accept,alt-a:select-all,alt-u:deselect-all,alt-u:deselect-all+accept,alt-enter:print-query"'

if [ "${SHELL_NAME}" = "zsh" ]; then

  fzf_choose_script() {
    local script_to_run=$(ssel)
    LBUFFER="${LBUFFER}${script_to_run}"
    local ret=$?
    zle redisplay
    typeset -f zle-line-init >/dev/null && zle zle-line-init
    return $ret
  }


  zle     -N    fzf_choose_script
  bindkey '\er' fzf_choose_script

  fzf_choose_command() {
    local command_to_run=$((for dir in $path; do
    ls $dir 
    done && (alias | cut -d = -f 1)) | \
      sort | \
      fzf --preview '(man {} 2>/dev/null) || (cat $(which {}) 2>/dev/null) || echo "No clue about: {}"')
      LBUFFER="${LBUFFER}${command_to_run}"
      local ret=$?
      zle redisplay
      typeset -f zle-line-init >/dev/null && zle zle-line-init
      return $ret
  }

  zle     -N    fzf_choose_command
  bindkey '\ee' fzf_choose_command

  fzf-git-show() {
    local out shas sha q k

    if [[ -d .git ]] || git rev-parse --git-dir > /dev/null 2>&1; then
      while out=$(
          git log --graph --color=always \
              --format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
          fzf --ansi --multi --no-sort --reverse --query="$q" \
              --print-query \
              --expect=ctrl-d,ctrl-l,ctrl-n \
              --toggle-sort=\`); do

        q=$(head -1 <<< "$out")
        k=$(head -2 <<< "$out" | tail -1)
        shas=$(sed '1,2d;s/^[^a-z0-9]*//;/^$/d' <<< "$out" | awk '{print $1}')

        [[ -z "$shas" ]] && continue

        case "$k" in
          ctrl-d)
            git diff --color=always $shas | less -R;;
          ctrl-l)
            git log -p --color=always ${shas}.. | less -R;;
          ctrl-n)
            git show --name-status --color=always ${shas} | less -R;;
          *)
            for sha in $shas; do
              git show --color=always $sha | less -R
            done
            ;;
        esac
      done
    else
      echo -e "Not a git repo"
    fi

    zle accept-line
  }

  zle     -N    fzf-git-show
  bindkey '\eq' fzf-git-show
fi


alias cd.p='cdf -d $p --fd-provider fd_project_dirs'
alias cd.f='dir="$(fd -t d | fzf --height=20%)"; [ -d "$dir"] && cd "$dir"'
alias .so="source '${HOME}/.${SHELL_NAME}rc'"


#set emacs mode
set -o emacs 

if [ "$SHELL_NAME" = "bash" -o "$SHELL_NAME" = "zsh" ] ; then
  for source_file in $(ls "$cfg_sh/$SHELL_NAME"); do
    source "$cfg_sh/$SHELL_NAME/$source_file" 
  done
fi
