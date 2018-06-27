#!/bin/bash

config_msg() {
  echo "Configuring: $@"
  echo '------------------------------------------------------------'
}

config_msg 'Home'

dot_dir='home'
for dot_file in $(cd $dot_dir && ls -1); do
  echo "${dot_file}: Linking file"

  symbolic=""
  if [[ "$dot_file" = 'gitconfig' ]]; then
    symbolic="-s"
  fi
  file_dest="${HOME}/.${dot_file}" 
  rm "${file_dest}"
  ln $symbolic "$(pwd)/${dot_dir}/${dot_file}" "${file_dest}"
done


config_msg 'Node Tools'

node_tools="node_tools"
(
  cd $node_tools; \
  yarn install; \
)
ln -s "$(pwd)/${node_tools}" "${HOME}/${node_tools}"

