download () {
  cd ~/Downloads
  curl -O "$@"
  cd -
}

pre_search () {
  if [ -z "$(echo $1 | sed 's/ //g')" ]; then
    read "search?Search: " 
    echo $search 
  else
    echo $@
  fi
}

ssh_to_tmux () {
  local extra=''
  if [[ "$3" != "" ]]; then
    local extra="send-keys '$3' Enter"
  fi

  ssh $1 -t "tmux new -s \"\$(whoami)\" \\; set -g status-bg $2 \\; $extra || tmux attach -t \"\$(whoami)\" \\; $extra "
}


search () {
  ARGS="$(pre_search $@)"
  xdg-open "https://duckduckgo.com?q=${ARGS}"
}

github_get_search() {
  for i in $(seq 2); do
    curl -s -X GET "https://api.github.com/search/repositories?page=${i}&per_page=100&q=$@" | jq -r '.items[].full_name'  
  done
}

select_github_repositories () {
  ARGS=$(pre_search $@)
  github_get_search $ARGS | fzf 
}

github_repos () {
  curl -s \
    -H "Authorization: Bearer ${GH_TOKEN}" \
    -X POST \
    -d "{ \"query\": \" \
     query { \
      search(query: \\\"$1\\\", type: REPOSITORY, first:50) { \
        repositoryCount \
        pageInfo { \
          endCursor \
          startCursor \
        } \
        edges { \
          node { \
            ... on Repository { \
              nameWithOwner \
            } \
          } \
        } \
      } \
    }  \
    \" }" \
    "${GH_BASE}/api/graphql" \
    | jq -r '.data.search.edges[].node | .nameWithOwner' \
    | sort
}

man () {
  /usr/bin/man $@ || (help $@ 2> /dev/null && help $@ | less)
}


wiki() {
  local query="${@}"
  w3m "https://en.wikipedia.org/w/index.php?search=${query}"
}

ddg() {
  local query="${@}"
  w3m "https://ddg.gg/?q=${query}"
}

jcat () {
  cat $@ | jq .
}

g_fdiff () {
  if [ -z "$1" ]; then
    exit; 
  fi
  git --no-pager diff --name-status $1 | fzf -m --preview "cd $(git rev-parse --show-toplevel) && git --no-pager  diff --color=always $1 -- {-1}"
}

csvcutf() {
  csvcut -c "$(csvcut -n $1 | fzf -m | egrep -oh '^\s*[0-9]*' | sed -e 's/\s//g' | head -c -1 | tr '\n' ',')" $1 
}



calc () {
  if [ -z "$1" ]; then
    julia 
  else
    julia -E "$(echo $@)"
  fi
}

fdf() {
  fd $@ | fzf -m
}


fd_dir() {
  local use_rel=false
  local use_exclude=true

  while true; do
    case "$1" in
      -d | --base-dir)
        local dir="$2"
        shift 2
        ;;
      --depth)
        local depth="$2"
        shift 2
        ;;
      -fp | --fd-params)
        local fd_params="$2" 
        shift 2
        ;;
      -s | --search)
        local search="$2" 
        shift 2
        ;;
      --no-exclude)
        local use_exclude=false
        shift
        ;;
      --exclude)
        local exclude="-E $2"
        shift 2
        ;;
        *)
        break
        ;;
    esac
  done

  if $use_exclude && [ -z "$exclude" ]; then
    local exclude="$(get_project_pattern --prefix -E --split) $(get_build_pattern --prefix -E --split)"
  fi

  eval "(cd ${dir:-.} && fd -HIL -d ${depth:-7} -t d ${exclude} ${fd_params} '(${search})')"
}


fd_dirf() {
  local use_rel=false
  local fd_provider="fd_dir"
  while true; do
    case "$1" in
      --fzf-opts)
        local fzf_opts="$2"
        shift 2
        ;;
      --use-relative)
        local use_rel=true
        shift 
        ;;
      --fd-provider)
        local fd_provider="$2"
        shift 2
        ;;
        *)
        break
        ;;
    esac
  done

  { 
    $fd_provider $@
    if  $use_rel ; then echo -e '.\n..'; fi
  } | fzf `echo $fzf_opts`
}


cdf() {
  while true; do
    case "$1" in
      --fzf-opts)
        local fzf_opts="$2"
        shift 2
        ;;
      -d | --base-dir)
        local dir="$2"
        shift 2
        ;;
        *)
        break
        ;;
    esac
  done

  local fzf_opts="--height=20 $fzf_opts"
  local target_dir=${dir:-${$(find_project_root):-.}}
  local sel_dir=$(fd_dirf --fzf-opts "$fzf_opts" "$@" -d "$target_dir") 
  [ ! -z "$sel_dir" ] && cd $target_dir/$sel_dir 
}

# Returns all directories (recursively) that contain a 'project' 
# 1. fd_dir looking for project markers 
# 2. then strips the project markers (including if ther are at the current directory level) 
# 3. sorts & dedups
fd_project_dirs() {
  local project_pattern="($(get_project_pattern --regex))$"
  fd_dir $@ -s $project_pattern --exclude $(get_build_pattern) \
    | sed 's!/[^/]*$!!' \
    | sed "s!$(echo $project_pattern | sed 's/\([()|]\)/\\\1/g')!.!" \
    | sort \
    | uniq
}

fd_git_dirs() {
  local project_pattern="(\.git)$"
  fd_dir $@ -s $project_pattern --exclude $(get_build_pattern) \
    | sed 's!/[^/]*$!!' \
    | sed "s!$(echo $project_pattern | sed 's/\([()|]\)/\\\1/g')!.!" \
    | sort \
    | uniq
}

explain () {
    if [ "$#" -eq 0 ]; then
        while read  -p "Command: " cmd; do
            curl -Gs "https://www.mankier.com/api/explain/?cols="$(tput cols) --data-urlencode "q=$cmd"
        done
        echo -e "\n"
    elif [ "$#" -eq 1 ]; then
        curl -Gs "https://www.mankier.com/api/explain/?cols="$(tput cols) --data-urlencode "q=$1"
    else
        echo "Usage"
        echo "explain                  interactive mode."
        echo "explain 'cmd -o | ...'   quoted command to be explainedt."
    fi
}

mv_up() {
  if [ -z "$1" ]; then
    return 
  fi

  local dest_dir="$1"
  local seperator="-"
  local dir_prefix="${dest_dir}${seperator}"

  mkdir "$dest_dir" && \
  ls | grep "$dir_prefix" \
    | while read -r dir; do
      local dir_sans_prefix="$(echo $dir | sed "s/^$dir_prefix//")"
      mv "$dir" "./${dest_dir}/${dir_sans_prefix}"
    done
}

get_project_pattern() {
  local project_markers=(.git .svn .idea)
  tw_arg_format_pieces --pieces "$(echo "$project_markers[@]" | tr ' ' '|')" $@
}

get_build_pattern() {
  local build_dirs=(node_modules)
  tw_arg_format_pieces --pieces "$(echo "$build_dirs[@]" | tr ' ' '|')" $@
}

tw_arg_format_pieces() {
  local is_plain=false
  local is_regex=false
  local is_split=false
  local prefix=''
  local seperator=''

  while true; do
    case "$1" in
      --prefix)
        local prefix="$2 "
        shift 2
        ;;
      --split)
        local is_split=true
        shift
        ;;
      --regex)
        local is_regex=true
        shift
        ;;
      --pieces)
        eval $(echo "local pieces=($(echo "$2" | tr "|" ' '))")
        shift 2;;
        *)
        break
        ;;
    esac
  done

  if [ ${#pieces[@]} -eq 0 ]; then
    echo "Fail"
    return 
  fi

  if $is_split; then
    local seperator=" $prefix"
  elif $is_regex; then
    local seperator="|"
  fi

  local pattern="";
  for piece in "${pieces[@]}"; do
    local pattern_piece="$piece"

    if $is_regex; then 
      local pattern_piece="$(echo $pattern_piece | sed 's/\./\\./g')"
    fi

    if [ "$pattern" != "" ]; then
      local pattern_piece="${seperator}${pattern_piece}" 
    fi

    local pattern="${pattern}${pattern_piece}"
  done

  local pattern="${prefix}${pattern}"

  echo $pattern;
}


find_parent_dir_with() {
  if [ -z "$1" ]; then
    echo "Must pass a pattern" 
    return
  fi

  local dir="";

  while [ "$(pwd)" != "/" ]; do
    if [ ! -z "$(ls -a | egrep "/(${1})$")" ]; then
      pwd
      break
    else
      cd ..
    fi
  done
}

find_project_root() {
  find_parent_dir_with "$(get_project_pattern --regex)"
}

