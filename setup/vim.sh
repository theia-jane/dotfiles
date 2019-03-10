#bin/bash

VIM_INIT="${cfg}/vim/init.vim"
VIMRC="${HOME}/.vimrc"
[ -f "${VIMRC}" ] &&  rm "${VIMRC}"
ln -s "${VIM_INIT}" "${VIMRC}"

NVIM_DIR="${cfg}/nvim"
mkdir -p "${NVIM_DIR}"

NVIM_INIT="${NVIM_DIR}/init.vim"
[ -f "${NVIM_INIT}" ] && rm "${NVIM_INIT}"
ln -s "${VIM_INIT}" "${NVIM_INIT}"

nvim -c "PlugInstall"
nvim -c "UpdateRemotePlugins"
