#!/bin/bash

path_dir="${HOME}/.config/path"
mkdir -p ${path_dir}
[ -e "${path_dir}/00-scripts" ] && rm "${path_dir}/00-scripts"
ln -s "${PWD}/src/scripts" "${path_dir}/00-scripts" 
