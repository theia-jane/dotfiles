#!/bin/bash

for dotfile in $(ls -1 ./src); do
  if [ -f "./src/${dotfile}" ]; then
    echo "${dotfile}: Linking dotfile"

    symbolic=""
    if [[ "$dotfile" = 'gitconfig' ]]; then
      symbolic="-s"
    fi
    file_dest="${HOME}/.${dotfile}" 
    rm "${file_dest}"
    ln $symbolic "$(pwd)/src/${dotfile}" "${file_dest}"
  fi
done
