#!/bin/bash
INIT=${PWD}/src/config/sh/init.sh
PROFILE=${HOME}/.profile
ZSHRC=${HOME}/.zshrc
BASHRC=${HOME}/.bashrc

[ -e "${PROFILE}" ] && rm ${PROFILE}; 
ln ${INIT} ${PROFILE}

[ -e "${ZSHRC}" -o -L "${ZSHRC}" ] && rm ${ZSHRC};
ln ${INIT} ${ZSHRC}

[ -e "${BASHRC}" -o -L "${BASHRC}" ] && rm ${BASHRC}; 
ln ${INIT} ${BASHRC}
