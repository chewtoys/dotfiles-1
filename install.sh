#!/bin/sh
# set -x

ln -sf $(pwd)/bin_files ~/bin
ln -sf $(pwd)/dircolors ~/.dircolors
ln -sf $(pwd)/gemrc ~/.gemrc
ln -sf $(pwd)/gitconfig ~/.gitconfig
ln -sf $(pwd)/gitignore ~/.gitignore
ln -sf $(pwd)/inputrc ~/.inputrc
ln -sf $(pwd)/tmux.conf ~/.tmux.conf
ln -sf $(pwd)/colordiffrc ~/.colordiffrc
ln -sf $(pwd)/ediff-merge-script /usr/local/bin/ediff-merge-script
ln -sf $(pwd)/aliases ~/.aliases
ln -sf $(pwd)/bash_files ~/.bash
ln -sf $(pwd)/bashrc ~/.bashrc
ln -sf $(pwd)/bash_profile ~/.bash_profile

cp -r $(pwd)/stack ~/.stack
