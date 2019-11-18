#!/bin/bash

# Toggle floating emacs frame in i3, or start if non-existing.

name='Dropdown: Emacs'
if [ ! -z "$@" ]; then
  name="${name} ($(echo "$@" | md5sum | cut -f1 -d' '))"
fi

if xwininfo -tree -root | grep "\"${name}\": (";
then
	echo "Window detected."
	i3-msg "[title=\"^${name}\"] scratchpad show"
else
	echo "Window not detected... spawning."
  emacsdclient -c -F '((width . 120) (height . 40) (name . "'"$name"'"))' "$@" 
fi

# vim: ft=sh
