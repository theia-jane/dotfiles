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
