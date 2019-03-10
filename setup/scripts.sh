#!/bin/bash

path_dir="${HOME}/.config/path"
mkdir -p ${path_dir}
ln -s ${PWD}/src/scripts ${path_dir}/00-scripts 
