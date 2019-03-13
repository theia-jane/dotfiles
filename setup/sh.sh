#!/bin/bash
SH_PATH=${PWD}/src/config/sh
INIT=${SH_PATH}/init.sh
PROFILE=${HOME}/.profile
ZSHRC=${HOME}/.zshrc
BASHRC=${HOME}/.bashrc

[ -e "${PROFILE}" ] && rm ${PROFILE}; 
ln ${INIT} ${PROFILE}

[ -e "${ZSHRC}" -o -L "${ZSHRC}" ] && rm ${ZSHRC};
ln ${INIT} ${ZSHRC}

[ -e "${BASHRC}" -o -L "${BASHRC}" ] && rm ${BASHRC}; 
ln ${INIT} ${BASHRC}

env_dir="${HOME}/.config/env"
[ ! -e "${env_dir}" ] && mkdir -p "${env_dir}"

env_file="${env_dir}/00-env"
[ -e "${env_file}" ] && rm "${env_file}"
ln ${SH_PATH}/env.sh "${env_file}"
