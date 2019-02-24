#bin/bash


rm ~/.vimrc
ln -s  $cfg/vim/init.vim ~/.vimrc

mkdir -p ~/.config/nvim
rm ~/.config/nvim/init.vim
ln -s  $cfg/vim/init.vim ~/.config/nvim/init.vim


nvim -c "PlugInstall"
nvim -c "UpdateRemotePlugins"
