#!/usr/bin/env bash

ABS_PATH=`pwd`

sudo ln -Tsi "$ABS_PATH/scripts/ecli" /usr/local/bin/ecli
sudo ln -Tsi "$ABS_PATH/scripts/wallpapers.py" /usr/local/bin/wallpapers

ln -Tsi "$ABS_PATH/conky" ~/.config/conky
ln -Tsi "$ABS_PATH/flake8/flake8" ~/.config/flake8
ln -Tsi "$ABS_PATH/ghci/ghci.conf" ~/.ghc/ghci.conf
ln -Tsi "$ABS_PATH/git/gitconfig" ~/.gitconfig
ln -Tsi "$ABS_PATH/systemd/user" ~/.config/systemd/user
ln -Tsi "$ABS_PATH/terminator/config" ~/.config/terminator/config
ln -Tsi "$ABS_PATH/vim" ~/.vim
ln -Tsi "$ABS_PATH/vimperator/vimperatorrc" ~/.vimperatorrc
ln -Tsi "$ABS_PATH/zsh/zshenv" ~/.zshenv
ln -Tsi "$ABS_PATH/zsh/zshrc" ~/.zshrc
