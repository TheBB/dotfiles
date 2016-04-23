#!/usr/bin/python
# -*- coding: utf-8 -*-

from random import choice
from os.path import dirname, join, realpath

SIGNATURE = """\
Eivind Fonn - http://www.aligulac.com/<br>
SINTEF IKT Anvendt Matematikk<br>
Strindveien 4, 7034 Trondheim<br><br>
"""

root = dirname(realpath(__file__))

with open(join(root, 'signature.txt'), 'w') as out:
    out.write(SIGNATURE)
    with open(join(root, 'signatures.txt'), 'r') as quotes:
        out.write(choice(quotes.readlines()))
