#!/bin/bash

for dot_file in $(git ls-files | grep '^\.'); do
echo "${dot_file}: Linking file"

file_dest="${HOME}/${dot_file}" 
rm "${file_dest}"
ln "${dot_file}" "${file_dest}"
done


