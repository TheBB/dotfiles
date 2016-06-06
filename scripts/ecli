#!/usr/bin/bash

if [[ "$#" -lt 1 ]] ; then
    emacsclient -c -e \
                "(select-frame-set-input-focus (selected-frame))" \
                "(delete-other-windows)" \
                "(spacemacs/home)"
else
    emacsclient -c -e \
                "(select-frame-set-input-focus (selected-frame))" \
                "(delete-other-windows)" \
                "(find-file \"$1\")"
fi
