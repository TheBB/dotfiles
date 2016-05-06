#!/usr/bin/env bash

ABS_PATH=`pwd`

sudo ln -Tsf "$ABS_PATH/scripts/ecli" /usr/local/bin/ecli
sudo ln -Tsf "$ABS_PATH/scripts/wallpapers.py" /usr/local/bin/wallpapers

ln -Tsf "$ABS_PATH/scripts/conky.desktop" ~/.config/autostart/conky.desktop

ln -Tsf "$ABS_PATH/conky" ~/.config/conky
ln -Tsf "$ABS_PATH/flake8/flake8" ~/.config/flake8
ln -Tsf "$ABS_PATH/ghci/ghci.conf" ~/.ghc/ghci.conf
ln -Tsf "$ABS_PATH/git/gitconfig" ~/.gitconfig
ln -Tsf "$ABS_PATH/systemd/user" ~/.config/systemd/user
ln -Tsf "$ABS_PATH/terminator/config" ~/.config/terminator/config
ln -Tsf "$ABS_PATH/vim" ~/.vim
ln -Tsf "$ABS_PATH/vimperator/vimperatorrc" ~/.vimperatorrc
ln -Tsf "$ABS_PATH/zsh/zshenv" ~/.zshenv
ln -Tsf "$ABS_PATH/zsh/zshrc" ~/.zshrc
