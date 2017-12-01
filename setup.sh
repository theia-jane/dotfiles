#!/bin/bash

dot_dir='./home'
for dot_file in $(cd $dot_dir && ls -1); do
echo "${dot_file}: Linking file"

file_dest="${HOME}/.${dot_file}" 
rm "${file_dest}"
ln "${dot_dir}/${dot_file}" "${file_dest}"
done


