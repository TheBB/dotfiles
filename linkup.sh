#!/usr/bin/env bash

ABS_PATH=`pwd`

if [[ "$1" = "--sudo" ]]; then
    sudo ln -Tsf "$ABS_PATH/scripts/ecli" /usr/local/bin/ecli
    sudo ln -Tsf "$ABS_PATH/scripts/prisma-puzzle-timer-synced" /usr/local/bin/prisma-puzzle-timer-synced
    sudo ln -Tsf "$ABS_PATH/scripts/ct-synced" /usr/local/bin/ct-synced
    sudo ln -Tsf "$ABS_PATH/scripts/wallpapers.py" /usr/local/bin/wallpapers
fi

mkdir -p ~/.config/autostart
mkdir -p ~/.config/systemd
mkdir -p ~/.config/terminator
mkdir -p ~/.ghc

ln -Tsf "$ABS_PATH/scripts/conky.desktop" ~/.config/autostart/conky.desktop

ln -Tsf "$ABS_PATH/conky" ~/.config/conky
ln -Tsf "$ABS_PATH/flake8/flake8" ~/.config/flake8
ln -Tsf "$ABS_PATH/ghci/ghci.conf" ~/.ghc/ghci.conf
ln -Tsf "$ABS_PATH/git/gitconfig" ~/.gitconfig
ln -Tsf "$ABS_PATH/git/gitignore_global" ~/.gitignore_global
ln -Tsf "$ABS_PATH/gtags/gtags.conf" ~/.globalrc
ln -Tsf "$ABS_PATH/systemd/user" ~/.config/systemd/user
ln -Tsf "$ABS_PATH/terminator/config" ~/.config/terminator/config
ln -Tsf "$ABS_PATH/terminology" ~/.config/terminology
ln -Tsf "$ABS_PATH/vim" ~/.vim
ln -Tsf "$ABS_PATH/vimperator/vimperatorrc" ~/.vimperatorrc
ln -Tsf "$ABS_PATH/zsh/zshenv" ~/.zshenv
ln -Tsf "$ABS_PATH/zsh/zshrc" ~/.zshrc
ln -Tsf "$ABS_PATH/zsh/zprofile" ~/.zprofile

dconf load /com/gexperts/Tilix/ < $ABS_PATH/dconf/tilix.dconf
