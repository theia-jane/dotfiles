#!/bin/bash

files_to_edit="$@"
if [ -z "$files_to_edit" ]; then
  files_to_edit="$(fzf -m)"
fi

(emacsclient -c "$files_to_edit" &) > /dev/null 2>&1 

# vim: ft=sh
