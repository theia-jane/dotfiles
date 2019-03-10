#!/bin/bash 

config_base="$(pwd)/src/config"

for config_dir in $(ls -1 "${config_base}"); do
  config_src="${config_base}/${config_dir}"
  if [ -d "${config_src}" ]; then
    echo "${config_dir}: Linking .config directory"
    dir_dest="${HOME}/.config/${config_dir}" 
    [ -e "${dir_dest}" ] && rm -rf "${dir_dest}"
    ln -s "${config_src}" "${dir_dest}"
  fi
done
