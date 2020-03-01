#!/bin/bash

home_dir="$(pwd)/src/home"
for dot_src in $(ls ${home_dir}); do
  dot_src_path="${home_dir}/${dot_src}"
  dot_dest_path="${HOME}/.${dot_src}" 

  if [ -f "${dot_src_path}" ]; then
    echo "${dot_src}: Linking file"
    [ -f "${dot_dest_path}" ] \
      && echo "Deleting existing file: ${dot_dest_path}" \
      && rm "${dot_dest_path}"

    ln "${dot_src_path}" "${dot_dest_path}"
  elif [ -d "${dot_src_path}" ]; then
    echo "${dot_src}: Linking directory"
    if [ ! -e "${dot_dest_path}" ]; then
      ln -s "${dot_src_path}" "${dot_dest_path}"
    else
      echo "Skipping, directory exists: ${dot_dest_path}"
    fi
  else
    echo "${dot_src}: Doing nothing"
  fi
done
