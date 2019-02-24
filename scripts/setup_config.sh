#!/bin/bash 
for config_dir in $(ls -1 ./src/config); do
  if [ -d "./src/config/${config_dir}" ]; then
    echo "${config_dir}: Linking .config directory"

    dir_dest="${HOME}/.config/${config_dir}" 
    rm -rf "${dir_dest}"
    ln -s "$(pwd)/src/config/${config_dir}" "${dir_dest}"
  fi
done
