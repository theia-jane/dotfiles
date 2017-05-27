#!/bin/bash

for dot_file in $(git ls-files | grep '^\.'); do

file_dest="${HOME}/${dot_file}" 
if [ -f "${file_dest}" ]; then
  echo "${file_dest}: File exists, making a backup."
  mv "${file_dest}" "${file_dest}.bk"
fi

echo "${dot_file}: Linking file"
ln -s "${dot_file}" "${file_dest}"

done


