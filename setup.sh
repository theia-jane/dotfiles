#!/bin/bash

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


