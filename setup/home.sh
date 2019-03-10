#!/bin/bash

home_dir="$(pwd)/src/home"
for dotfile in $(ls ${home_dir}); do
  if [ -f "${home_dir}/${dotfile}" ]; then
    echo "${dotfile}: Linking dotfile"
    file_dest="${HOME}/.${dotfile}" 
    [ -f "${file_dest}" ] && rm "${file_dest}"
    ln "${home_dir}/${dotfile}" "${file_dest}"
  fi
done
