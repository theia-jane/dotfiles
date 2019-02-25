export SHELL_NAME="$(echo $0 | grep -oh '[a-z]*sh')"
export OS="$(uname | tr "[A-Z]" "[a-z]")"
export cfg="${HOME}/.config"
export cfg_sh="${cfg}/sh"

# files each
declare -a TARGET_FILES=("utils.sh"  "environment.sh" "init.sh" "options.sh" "functions.sh" "aliases.sh")

if [ "${1}" = "skip" ]; then
  declare -a TARGET_FILES=("utils.sh" "environment.sh")
fi

# allow various envionment differences: base, os & shell
declare -a ENV_CONDS=("base" "os/$OS" "shell/$SHELL_NAME")

# allow local overrides
declare -a TARGETS=("default" "local")

declare TOOL_SOURCE=""

for TARGET_FILE in ${TARGET_FILES[@]}; do
  for ENV_COND in ${ENV_CONDS[@]}; do
    for TARGET in ${TARGETS[@]}; do
      FILE="${cfg_sh}/${TARGET}/${ENV_COND}/${TARGET_FILE}"
      [ -e "$FILE" ] && source "$FILE"
    done 
  done
done 

for TOOL in $(echo $TOOL_SOURCE | tr ':' '\n'); do
  [ -e "$TOOL" ] && source "$TOOL"
done
