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

