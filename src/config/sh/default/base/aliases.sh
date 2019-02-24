alias g='git'
alias f='fzf --query'
alias path.f='echo $path | tr " " "\n" | fzf -m --preview "ls {}"'

alias filter.e='{ export t=$(mktemp) && (tee "$t" > /dev/null) } && nvim +"nnoremap q :wq<CR>" $t && { cat $t; rm $t; unset t}'
alias less="less -r"
alias patch.v="(v; echo -e '\n') | patch -p0"

alias ls.d="fd_dir"
alias ls.idea='fd_dir -s .idea --exclude node_modules'
alias ls.git='fd_dir -s .git --exclude node_modules'

alias e.v='nvim `v`'
alias e="nvim"
alias ef='nvim -c "FZF"'
alias er='nvim -c "History"'
alias edot="(cd $cfg/dotfiles && ef)"
alias esh="(cd $cfg_sh && ef && .so)"
alias erg="(cd $org && ef)"
alias evim="(cd $cfg/vim && ef)"
alias esrc="(cd $s && ef)"
alias esnip="(cd $cfg/templates && ef)"

alias rmf="(fzf -m --preview='ccat {}' || exit 0) | xargs rm -rf"
alias p.root='[ ! -z "$(find_project_root)" ] && cd $(find_project_root) || echo "No project detected."'

alias g.root='[ ! -z "$(find_parent_dir_with .git)" ] && cd $(find_parent_dir_with .git) || echo "Not in a git repository"'
alias g.clean="(g.root && (git ls-files -md | xargs git reset HEAD) && (git ls-files -md | xargs git checkout --) && (git ls-files -o --exclude-standard | xargs rm -rf))"
alias g.ppull='(psel | while read -r dir; do; cd $dir && git pull; done)'
alias g.co='git branch --all | sed "s/remotes\/origin\///" | egrep -v -e "->" -e "$(git rev-parse --abbrev-ref HEAD --)" | sort | uniq | fzf | cut -c 3- | xargs -r git checkout'
alias g.ls="git status | grep '^\\s*[a-zA-Z]*:' | awk '{print \$(NF)}'"
alias g.fzf="g.ls | fzf -m --preview 'git diff HEAD {}'"
alias g.next.ci="git rev-list --reverse --ancestry-path  HEAD..master | head -n 1"
alias g.cif="git log --format=format:'%H %C(dim bold white)%an %Creset%s %C(white dim italic)%cr' \`echo \${git_fzf_format:-'--follow -- .'}\` | fzf -m --ansi --with-nth 2.. --preview 'git diff --color {1}~1 {1}' | awk '{print \$1}'"
alias g.af='git add `g.fzf`'
alias gh.select_repo='github_repo=$(select_github_repositories)'
alias gh.clone='(gh.select_repo && cd $p && git clone "https://github.com/${github_repo}")'
alias gh.view='(gh.select_repo && xdg-open "https://github.com/${github_repo}")'
alias g.gpull="(psel | while read -r dir; do; cd \$dir && git add . && git commit && git pull && git push; done)"
alias g.bclean='git branch -D $(git branch | fzf -m)'
alias g.spop='stash=$(git stash list | fzf --preview "git diff \$(echo {} | egrep -oh \"^[^:]*\")" | egrep -oh "^[^:]+") && git stash pop "$stash"'
alias g.sub_check='for dir in $(fd_git_dirs); do; (cd $dir && echo $dir && git status); done'
alias q.cli="jq -cR '[splits(\" +\")]' | jq -s '.'"

alias lcat="ls | fzf -m --preview='cat {}'"
alias v.jq="v | jq"

alias npm.run="jq -r '.scripts | keys[] as \$k | \"\(\$k), \(.[\$k])\"' package.json | fzf | awk 'BEGIN{FS=\",\"} {print \$1}' | xargs npm run"

alias cd.p='cdf -d $p --fd-provider fd_project_dirs'
alias cd.cfg="cdf -d \$cfg --fd-provider fd_project_dirs --fd-params \"-E emacsds\""
alias cd.f='cdf'

alias tmux.ls="tmux list-sessions"
alias tail.sys="tail -f /var/log/syslog"
