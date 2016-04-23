#!/usr/bin/env python3

from os import getenv, listdir
from os.path import join
from random import choice
from subprocess import run

root = join(getenv('HOME'), '.wallpapers')
wallpaper = choice(listdir(root))
run(['gsettings', 'set', 'org.gnome.desktop.background', 'picture-uri',
     join(root, wallpaper)])
